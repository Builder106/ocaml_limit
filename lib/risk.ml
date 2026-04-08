(** {1 Pre-Trade Risk Engine}

    Validates every incoming order against configurable risk limits before
    it reaches the matching engine. These checks run in the hot path and
    are therefore designed for minimal overhead — no allocation, no
    branching on heap-allocated data, pure integer arithmetic.

    {b Risk checks implemented}:
    - {b Fat-finger price guard}: Rejects orders whose price falls outside
      the configured [min_price, max_price] band.
    - {b Maximum order size}: Rejects orders exceeding [max_order_qty].
    - {b Position limit}: Tracks net position per-symbol and rejects orders
      that would breach [max_position].
    - {b Self-trade prevention}: Detects when an incoming order would match
      against the same participant (stub — requires participant ID). *)

open Types

(** Risk check result — uses a variant rather than exceptions to avoid
    the overhead of exception handling in the hot path. The [Ok] case
    carries no payload to avoid allocation. *)
type risk_result =
  | Pass
  | Reject_price_band
  | Reject_max_qty
  | Reject_position_limit

(** Mutable position tracker. Maintains the net position for a single
    symbol to enable real-time position limit checks. *)
type position_tracker = {
  mutable net_position : int;  (** Positive = long, negative = short *)
}

(** [create_tracker ()] creates a fresh position tracker at zero. *)
let create_tracker () = { net_position = 0 }

(** [check_price_band config order] validates that the order price is
    within the configured [min_price, max_price] band.

    {b Rationale}: Fat-finger errors (e.g., buying at $10,000 instead of
    $100.00) are a major source of risk in electronic trading. This check
    catches erroneous prices before they can cause a catastrophic fill.

    {b Complexity}: O(1), two integer comparisons, zero allocation. *)
let[@inline] check_price_band (config : config) (order : order) =
  if order.price < config.min_price || order.price > config.max_price then
    Reject_price_band
  else
    Pass

(** [check_max_qty config order] validates that the order quantity does
    not exceed the configured maximum.

    {b Complexity}: O(1), one integer comparison. *)
let[@inline] check_max_qty (config : config) (order : order) =
  if order.remaining_qty > config.max_order_qty then
    Reject_max_qty
  else
    Pass

(** [check_position_limit config tracker order] validates that filling
    this order would not breach the net position limit.

    This is a pessimistic check — it assumes the entire order will be
    filled immediately. In practice, partial fills would consume less
    position capacity, but the conservative approach is standard practice
    in production risk systems.

    {b Complexity}: O(1), integer arithmetic + comparison. *)
let[@inline] check_position_limit (config : config) (tracker : position_tracker) (order : order) =
  let projected = match order.side with
    | Buy  -> tracker.net_position + order.remaining_qty
    | Ask  -> tracker.net_position - order.remaining_qty
  in
  if abs projected > config.max_position then
    Reject_position_limit
  else
    Pass

(** [update_position tracker side qty] updates the position tracker after
    a fill. Called by the matching engine on every execution.

    {b Complexity}: O(1), one integer addition. *)
let[@inline] update_position tracker side qty =
  match side with
  | Buy  -> tracker.net_position <- tracker.net_position + qty
  | Ask  -> tracker.net_position <- tracker.net_position - qty

(** [validate config tracker order] runs the full pre-trade risk check
    pipeline. Returns [Pass] if all checks pass, or the first failing
    check's rejection reason.

    The checks are ordered by computational cost (cheapest first) to
    maximize early-exit probability.

    {b Complexity}: O(1) worst case, zero allocation. *)
let validate (config : config) (tracker : position_tracker) (order : order) =
  match check_max_qty config order with
  | Reject_max_qty as r -> r
  | _ -> (
      match check_price_band config order with
      | Reject_price_band as r -> r
      | _ -> check_position_limit config tracker order)

(** [risk_result_to_string r] returns a human-readable description of
    the risk check result. Used only in reporting/logging. *)
let risk_result_to_string = function
  | Pass -> "PASS"
  | Reject_price_band -> "REJECTED: price outside configured band"
  | Reject_max_qty -> "REJECTED: quantity exceeds maximum"
  | Reject_position_limit -> "REJECTED: would breach position limit"
