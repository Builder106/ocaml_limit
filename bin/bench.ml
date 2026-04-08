(** {1 Zero-Allocation Performance Benchmark}

    A synthetic load test that processes 1,000,000 orders to measure:
    1. Maximum throughput (Orders Per Second).
    2. Tail latency (p50, p99, p99.9).
    3. GC Impact (proves zero minor collections in the hot path). *)

open Ocaml_lob
open Types
module Engine = Engine
module Stats = Stats

let run_benchmark () =
  let config = default_config in
  let engine = Engine.create config in
  let stats = Stats.create 1_000_000 in
  let num_orders = 1_000_000 in

  (* Pre-allocate an order pool for the benchmark to avoid heap allocation in the loop *)
  let order_pool =
    Array.init num_orders (fun i ->
        let side = if i mod 2 = 0 then Buy else Ask in
        let price = 100_000 + (i mod 100) in
        make_order ~id:i ~side ~price ~qty:10 ~order_type:Limit ~timestamp:0)
  in

  let dummy_on_fill _ _ _ _ _ = () in

  Printf.printf "\n--- OCaml-LOB Benchmark: %d Orders ---\n" num_orders;

  let gc_before = Gc.stat () in
  let start_time = Unix.gettimeofday () in

  for i = 0 to num_orders - 1 do
    let order = order_pool.(i) in
    let t_start = Unix.gettimeofday () in
    let _ = Engine.submit engine order dummy_on_fill in
    let ns = (Unix.gettimeofday () -. t_start) *. 1_000_000_000.0 |> int_of_float in
    Stats.record stats ns
  done;

    let total_time = Unix.gettimeofday () -. start_time in
    let gc_after = Gc.stat () in

    (* Results *)
    let ops = float_of_int num_orders /. total_time in
    let p50 = Stats.get_percentile stats 0.50 in
    let p99 = Stats.get_percentile stats 0.99 in
    let p999 = Stats.get_percentile stats 0.999 in

    Printf.printf "Throughput: %.2f M orders/s\n" (ops /. 1_000_000.0);
    Printf.printf "Latency (p50): %.2f μs\n" p50;
    Printf.printf "Latency (p99): %.2f μs\n" p99;
    Printf.printf "Latency (p99.9): %.2f μs\n" p999;
    
    (* GC Analysis *)
    let minor_diff = gc_after.minor_collections - gc_before.minor_collections in
    let major_diff = gc_after.major_collections - gc_before.major_collections in
    
    Printf.printf "\n--- GC IMPACT ---\n";
    Printf.printf "Minor Collections: %d (Zero-Allocation Goal: 0)\n" minor_diff;
    Printf.printf "Major Collections: %d\n" major_diff;
    
    if minor_diff = 0 then
        Printf.printf "\n[STUNNING] ZERO-ALLOCATION CLAIM VALIDATED.\n"
    else
        Printf.printf "\n[WARNING] Allocations detected. Check the hot path.\n"

let () = run_benchmark ()
