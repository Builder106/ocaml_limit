(** {1 The Matching Engine Core}

    High-performance exchange-grade matching logic with O(log n) price-level
    access and O(1) FIFO order execution.

    {b Features}:
    - Full Limit Order Book (LOB) maintenance (Bids/Asks).
    - Limit, Iceberg, and Post-Only order types.
    - Zero-allocation hot path (reuses Pool nodes).
    - Price-Time Priority matching algorithm. *)

open Types
module Pool = Pool
module Risk = Risk

(** {2 Engine State} *)

type t = {
    book : order_book;
    pool : Pool.t;
    risk : Risk.position_tracker;
    config : config;
    mutable last_match_price : price;
}

(** [create config] initializes a fresh matching engine with pre-allocated pools. *)
let create config =
  let pool = Pool.create config.max_orders in
  let book = {
    bids = { levels = PriceMap.empty; best = 0 };
    asks = { levels = PriceMap.empty; best = 0 };
    nodes = pool.nodes;
    next_node = 0;
  } in
  { book; pool; risk = Risk.create_tracker (); config; last_match_price = 0 }

(** {2 Price Level Management} *)

let get_or_create_level side book_side price =
  match PriceMap.find_opt price book_side.levels with
  | Some level -> level
  | None ->
      let level = {
        pl_price = price;
        pl_total_qty = 0;
        pl_order_count = 0;
        pl_head = -1;
        pl_tail = -1;
      } in
      book_side.levels <- PriceMap.add price level book_side.levels;
      if book_side.best = 0 then book_side.best <- price
      else if (side = Buy && price > book_side.best) || (side = Ask && price < book_side.best)
      then book_side.best <- price;
      level

(** {2 The Matching Loop} *)

let match_aggressive engine order on_fill =
  let opposite_side = opposite_side order.side in
  let opposite_book = if opposite_side = Buy then engine.book.bids else engine.book.asks in

  let rec fill_loop () =
    if order.remaining_qty <= 0 then ()
    else if opposite_book.best = 0 then ()
    else
      let best_price = opposite_book.best in
      let can_match =
        match order.side with Buy -> order.price >= best_price | Ask -> order.price <= best_price
      in

      if not can_match then ()
      else
        let level = PriceMap.find best_price opposite_book.levels in
        let head_node_idx = level.pl_head in

        if head_node_idx = -1 then (
          opposite_book.levels <- PriceMap.remove best_price opposite_book.levels;
          opposite_book.best <-
            (match side_to_string opposite_side with
            | "BUY" -> (
                match PriceMap.max_binding_opt opposite_book.levels with
                | Some (p, _) -> p
                | None -> 0)
            | _ -> (
                match PriceMap.min_binding_opt opposite_book.levels with
                | Some (p, _) -> p
                | None -> 0));
          fill_loop ())
        else
          let node = Pool.get engine.pool head_node_idx in
          let passive = node.qn_order in
          let match_qty = min order.remaining_qty passive.remaining_qty in

          order.remaining_qty <- order.remaining_qty - match_qty;
          passive.remaining_qty <- passive.remaining_qty - match_qty;
          level.pl_total_qty <- level.pl_total_qty - match_qty;
          engine.last_match_price <- best_price;
          Risk.update_position engine.risk order.side match_qty;

          on_fill passive.id order.id best_price match_qty order.timestamp;

          if passive.remaining_qty = 0 then (
            passive.status <- Filled;
            level.pl_head <- node.qn_next;
            if level.pl_head = -1 then level.pl_tail <- -1
            else (Pool.get engine.pool level.pl_head).qn_prev <- -1;
            level.pl_order_count <- level.pl_order_count - 1;
            Pool.free engine.pool head_node_idx;

            match passive.order_type with
            | Iceberg { visible_qty; total_qty } ->
                let remaining_hidden = total_qty - passive.original_qty in
                if remaining_hidden > 0 then (
                  let reload_qty = min visible_qty remaining_hidden in
                  let reloaded_order =
                    {
                      passive with
                      remaining_qty = reload_qty;
                      original_qty = reload_qty;
                      order_type = Iceberg { visible_qty; total_qty = remaining_hidden };
                    }
                  in
                  let new_idx = Pool.alloc engine.pool reloaded_order in
                  let tail_idx = level.pl_tail in
                  if tail_idx = -1 then (
                    level.pl_head <- new_idx;
                    level.pl_tail <- new_idx)
                  else
                    let tail_node = Pool.get engine.pool tail_idx in
                    tail_node.qn_next <- new_idx;
                    (Pool.get engine.pool new_idx).qn_prev <- tail_idx;
                    level.pl_tail <- new_idx;
                  level.pl_total_qty <- level.pl_total_qty + reload_qty;
                  level.pl_order_count <- level.pl_order_count + 1)
                else ()
            | _ -> ())
          else ();
          fill_loop ()
  in
  fill_loop ()

(** {2 Public API} *)

let submit engine order on_fill =
  match Risk.validate engine.config engine.risk order with
  | Risk.Reject_price_band | Risk.Reject_max_qty | Risk.Reject_position_limit as r ->
      order.status <- Rejected;
      Error r
  | Risk.Pass ->
      let opposite_book = if order.side = Buy then engine.book.asks else engine.book.bids in
      let best_opp = opposite_book.best in
      let would_match =
        if best_opp = 0 then false
        else match order.side with Buy -> order.price >= best_opp | Ask -> order.price <= best_opp
      in

      if order.order_type = Post_only && would_match then (
        order.status <- Rejected;
        Error Risk.Reject_price_band)
      else (
        match_aggressive engine order on_fill;
        if order.remaining_qty > 0 then (
          let book_side = if order.side = Buy then engine.book.bids else engine.book.asks in
          let level = get_or_create_level order.side book_side order.price in
          let node_idx = Pool.alloc engine.pool order in
          let tail_idx = level.pl_tail in
          if tail_idx = -1 then (
            level.pl_head <- node_idx;
            level.pl_tail <- node_idx)
          else
            let tail_node = Pool.get engine.pool tail_idx in
            tail_node.qn_next <- node_idx;
            (Pool.get engine.pool node_idx).qn_prev <- tail_idx;
            level.pl_tail <- node_idx;
          level.pl_total_qty <- level.pl_total_qty + order.remaining_qty;
          level.pl_order_count <- level.pl_order_count + 1)
        else if order.status = Active then order.status <- Filled;
        Ok ())

(** [cancel engine order_id price side] removes an order from the book.
    O(log n) to find the price level, then O(1) to remove from the queue. *)
let cancel engine order_id price side =
    let book_side = if side = Buy then engine.book.bids else engine.book.asks in
    match PriceMap.find_opt price book_side.levels with
    | None -> false
    | Some level ->
        (* Scan the level (O(k)) — in a production system, we'd use a 
           hash-map of order_id -> node_idx for O(1) cancel. *)
        let rec find_and_remove idx =
            if idx = -1 then false
            else
                let node = Pool.get engine.pool idx in
                if node.qn_order.id = order_id then (
                    let prev_idx = node.qn_prev in
                    let next_idx = node.qn_next in
                    
                    if prev_idx <> -1 then (Pool.get engine.pool prev_idx).qn_next <- next_idx
                    else level.pl_head <- next_idx;
                    
                    if next_idx <> -1 then (Pool.get engine.pool next_idx).qn_prev <- prev_idx
                    else level.pl_tail <- prev_idx;
                    
                    level.pl_total_qty <- level.pl_total_qty - node.qn_order.remaining_qty;
                    level.pl_order_count <- level.pl_order_count - 1;
                    node.qn_order.status <- Cancelled;
                    Pool.free engine.pool idx;
                    true
                ) else find_and_remove node.qn_next
        in
        find_and_remove level.pl_head
