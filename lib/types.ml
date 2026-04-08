(** {1 Core Types for the Limit Order Book}

    All prices use fixed-point integer arithmetic to eliminate floating-point
    non-determinism and avoid boxing overhead. A price of [10050] represents
    [$1.0050] at 4-decimal-place precision (1 tick = 0.0001).

    This module defines the fundamental data structures that power the
    matching engine's O(log n) price-level access and O(1) FIFO execution. *)

(** {2 Fixed-Point Price Arithmetic} *)

(** Price represented as an integer in ticks (1 tick = 0.0001 currency units).
    For example, a price of $150.25 is stored as [1_502_500]. This avoids all
    floating-point issues and keeps prices unboxed on 64-bit systems. *)
type price = int

(** Quantity of shares/contracts. Stored as integer units. *)
type qty = int

(** Monotonically increasing order identifier. Used for time-priority
    ordering within a price level. *)
type order_id = int

(** Monotonically increasing timestamp in nanoseconds from an arbitrary epoch.
    Used for latency measurement and audit trails. *)
type timestamp = int

(** {2 Order Side} *)

type side =
  | Buy   (** Bid side — willing to buy at the given price or lower *)
  | Ask   (** Ask side — willing to sell at the given price or higher *)

(** {2 Order Types} *)

(** Distinguishes between standard limit orders and advanced order types
    that are commonly seen in institutional matching engines. *)
type order_type =
  | Limit
      (** Standard limit order: rests on the book at the specified price. *)
  | Iceberg of { visible_qty : qty; total_qty : qty }
      (** Iceberg order: only [visible_qty] is displayed. When the visible
          portion is fully filled, it reloads from [total_qty] remaining,
          losing time priority on reload. This models the hidden liquidity
          behavior seen on production exchange feeds. *)
  | Post_only
      (** Post-only (maker-only): the order is rejected if it would
          immediately match against resting liquidity. Used by market makers
          to guarantee rebate capture and avoid taker fees. *)

(** {2 Order Record}

    Mutable fields are used deliberately: the matching engine modifies
    [remaining_qty] and [status] in-place to avoid allocation during the
    hot path. This is a conscious trade-off of functional purity for
    zero-GC-pressure execution. *)

type order_status =
  | Active    (** Resting on the book, eligible for matching *)
  | Filled    (** Fully executed *)
  | Cancelled (** Cancelled by the participant *)
  | Rejected  (** Rejected by pre-trade risk checks *)

type order = {
  id : order_id;
  side : side;
  price : price;
  original_qty : qty;
  mutable remaining_qty : qty;
  order_type : order_type;
  mutable status : order_status;
  timestamp : timestamp;
}

(** {2 Execution Report}

    Produced by the engine on every fill event. These are value types
    (immutable records) because they leave the hot path immediately —
    they are consumed by the reporting/logging layer which is not
    latency-sensitive. *)

type fill = {
  fill_order_id : order_id;     (** The resting order that was filled *)
  fill_incoming_id : order_id;  (** The incoming aggressive order *)
  fill_price : price;
  fill_qty : qty;
  fill_timestamp : timestamp;
}

(** {2 Price Level (FIFO Queue)}

    Each price level maintains a doubly-linked list of orders for O(1)
    insertion at the tail and O(1) removal from the head during matching.
    We implement this as a simple mutable queue backed by an array to
    keep the memory layout cache-friendly. *)

(** A node in the price-level queue. Uses index-based "pointers" into
    a pre-allocated array rather than heap-allocated linked list nodes. *)
type queue_node = {
  mutable qn_order : order;
  mutable qn_next : int;  (** Index of next node, or -1 for tail *)
  mutable qn_prev : int;  (** Index of prev node, or -1 for head *)
}

(** The FIFO queue at a single price level. *)
type price_level = {
  pl_price : price;
  mutable pl_total_qty : qty;       (** Sum of remaining_qty at this level *)
  mutable pl_order_count : int;     (** Number of active orders *)
  mutable pl_head : int;            (** Index of first node, or -1 *)
  mutable pl_tail : int;            (** Index of last node, or -1 *)
}

(** {2 Order Book}

    The top-level order book uses OCaml's stdlib [Map] (a balanced binary
    search tree, specifically a height-balanced AVL tree) for price-level
    lookup. This guarantees O(log n) insertion, deletion, and best-price
    access where n is the number of distinct price levels.

    We maintain separate maps for bids and asks:
    - Bids: sorted descending (best bid = max key)
    - Asks: sorted ascending (best ask = min key) *)

module PriceMap = Map.Make (Int)

type book_side = {
  mutable levels : price_level PriceMap.t;
  mutable best : price;  (* Cached best price. 0 = no liquidity. *)
}

type order_book = {
  bids : book_side;
  asks : book_side;
  (** Node pool: pre-allocated array of queue nodes to avoid per-order
      heap allocation during the matching hot path. *)
  nodes : queue_node array;
  mutable next_node : int;  (** Next free slot in the node pool *)
}

(** {2 Engine Configuration} *)

type config = {
  max_orders : int;           (** Size of the pre-allocated node pool *)
  tick_size : price;          (** Minimum price increment (e.g., 1 = 0.0001) *)
  max_price : price;          (** Upper bound for fat-finger checks *)
  min_price : price;          (** Lower bound for fat-finger checks *)
  max_order_qty : qty;        (** Maximum single-order quantity *)
  max_position : qty;         (** Maximum net position per symbol *)
}

(** Default configuration suitable for benchmarking. *)
let default_config = {
  max_orders = 1_000_000;
  tick_size = 1;
  max_price = 100_000_000;   (* $10,000.00 *)
  min_price = 1;              (* $0.0001 *)
  max_order_qty = 1_000_000;
  max_position = 10_000_000;
}

(** {2 Helper Functions} *)

(** [make_order ~id ~side ~price ~qty ~order_type ~timestamp] creates a new
    order value. This is the only allocation point for orders — it happens
    at the gateway boundary, outside the matching hot path. *)
let make_order ~id ~side ~price ~qty ~order_type ~timestamp =
  { id; side; price; original_qty = qty; remaining_qty = qty;
    order_type; status = Active; timestamp }

(** [price_to_float p] converts a fixed-point price to a float for display.
    Only used in reporting — never in the matching hot path. *)
let price_to_float p = Float.of_int p /. 10_000.0

(** [float_to_price f] converts a float to a fixed-point price.
    Used at the gateway boundary for order entry. *)
let float_to_price f = Float.to_int (f *. 10_000.0)

(** [opposite_side s] returns the other side of the market. *)
let opposite_side = function
  | Buy -> Ask
  | Ask -> Buy

(** [side_to_string s] returns a human-readable string for a side. *)
let side_to_string = function
  | Buy -> "BUY"
  | Ask -> "ASK"

(** [status_to_string s] returns a human-readable string for an order status. *)
let status_to_string = function
  | Active -> "ACTIVE"
  | Filled -> "FILLED"
  | Cancelled -> "CANCELLED"
  | Rejected -> "REJECTED"
