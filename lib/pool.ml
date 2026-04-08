(** {1 Pre-Allocated Object Pool}

    Eliminates per-order heap allocation in the matching hot path by
    maintaining a free-list over a pre-allocated array of queue nodes.

    {b Design rationale}: In a GC'd language, the primary source of
    latency jitter is minor-heap promotion and subsequent major GC pauses.
    By pre-allocating all node storage at initialization time and recycling
    slots via an index-based free-list, the matching engine's inner loop
    triggers zero GC minor collections.

    The pool uses a simple stack-based free-list: freed indices are pushed
    onto [free_stack] and popped on allocation. This gives O(1) alloc/free
    with no heap allocation. *)

open Types

(** The object pool state. *)
type t = {
  nodes : queue_node array;
  free_stack : int array;  (** Stack of free node indices *)
  mutable free_top : int;  (** Index of the top of the free stack (-1 = empty) *)
  mutable allocated : int; (** Number of currently allocated nodes *)
  capacity : int;
}

(** Sentinel order used to initialize pool slots. Never participates
    in matching — exists only to satisfy the type system without
    using [option] (which would box). *)
let sentinel_order : order = {
  id = -1;
  side = Buy;
  price = 0;
  original_qty = 0;
  remaining_qty = 0;
  order_type = Limit;
  status = Cancelled;
  timestamp = 0;
}

(** Sentinel node used to fill the pool array at initialization. *)
let sentinel_node : queue_node = {
  qn_order = sentinel_order;
  qn_next = -1;
  qn_prev = -1;
}

(** [create capacity] allocates a pool of [capacity] queue nodes.
    This is the {b only} allocation point in the system — it happens
    once at startup, before any orders arrive. *)
let create capacity =
  let nodes = Array.make capacity sentinel_node in
  (* Initialize each slot with its own mutable copy *)
  for i = 0 to capacity - 1 do
    nodes.(i) <- {
      qn_order = sentinel_order;
      qn_next = -1;
      qn_prev = -1;
    }
  done;
  let free_stack = Array.init capacity (fun i -> capacity - 1 - i) in
  { nodes; free_stack; free_top = capacity - 1; allocated = 0; capacity }

(** [alloc pool order] claims a node from the pool and initializes it
    with [order]. Returns the node index, or raises [Failure] if the
    pool is exhausted (which should never happen in a well-sized system).

    {b Complexity}: O(1), zero heap allocation. *)
let alloc pool order =
  if pool.free_top < 0 then
    failwith "Pool: exhausted — increase max_orders in config"
  else begin
    let idx = pool.free_stack.(pool.free_top) in
    pool.free_top <- pool.free_top - 1;
    pool.allocated <- pool.allocated + 1;
    let node = pool.nodes.(idx) in
    node.qn_order <- order;
    node.qn_next <- -1;
    node.qn_prev <- -1;
    idx
  end

(** [free pool idx] returns the node at [idx] to the pool.

    {b Complexity}: O(1), zero heap allocation. *)
let free pool idx =
  pool.free_top <- pool.free_top + 1;
  pool.free_stack.(pool.free_top) <- idx;
  pool.allocated <- pool.allocated - 1;
  (* Reset the node to prevent dangling references *)
  let node = pool.nodes.(idx) in
  node.qn_order <- sentinel_order;
  node.qn_next <- -1;
  node.qn_prev <- -1

(** [get pool idx] returns the queue node at [idx].
    {b Complexity}: O(1) array access. *)
let[@inline] get pool idx = pool.nodes.(idx)

(** [utilization pool] returns the fraction of the pool currently in use.
    Used for capacity monitoring. *)
let utilization pool =
  Float.of_int pool.allocated /. Float.of_int pool.capacity

(** [stats pool] returns [(allocated, capacity)] for reporting. *)
let stats pool = (pool.allocated, pool.capacity)
