(** {1 Performance Statistics & Latency Tracking}

    Tracks execution latency and throughput using nanosecond-resolution
    timers. Optimized for zero-allocation recording. *)

type t = {
    mutable total_orders : int;
    mutable start_time : float;
    latencies : int array; (* Array of latencies in nanoseconds *)
    mutable lat_idx : int;
}

let create capacity = {
    total_orders = 0;
    start_time = Unix.gettimeofday ();
    latencies = Array.make capacity 0;
    lat_idx = 0;
}

let record t ns =
    t.total_orders <- t.total_orders + 1;
    if t.lat_idx < Array.length t.latencies then (
        t.latencies.(t.lat_idx) <- ns;
        t.lat_idx <- t.lat_idx + 1
    )

let get_percentile t p =
    if t.lat_idx = 0 then 0.0
    else
        let sorted = Array.sub t.latencies 0 t.lat_idx in
        Array.sort compare sorted;
        let idx = (float_of_int (t.lat_idx - 1)) *. p |> int_of_float in
        float_of_int sorted.(idx) /. 1000.0 (* return in microseconds *)

let get_throughput t =
    let elapsed = Unix.gettimeofday () -. t.start_time in
    if elapsed <= 0.0 then 0.0
    else float_of_int t.total_orders /. elapsed /. 1_000_000.0 (* M orders/s *)

let reset t =
    t.total_orders <- 0;
    t.start_time <- Unix.gettimeofday ();
    t.lat_idx <- 0
