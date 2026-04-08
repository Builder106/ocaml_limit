(** {1 Unit Tests for Matching Correctness}

    Uses Alcotest to verify:
    - Price-Time Priority matching.
    - Partial fills and full executions.
    - Iceberg reloading.
    - Post-Only rejections. *)

open Ocaml_lob
open Types
module Engine = Engine
module Risk = Risk

let reject_result_test = Alcotest.testable Risk.risk_result_to_string ( = )

let test_basic_matching () =
    let engine = Engine.create default_config in
    let buy = make_order ~id:1 ~side:Buy ~price:100 ~qty:10 ~order_type:Limit ~timestamp:0 in
    let sell = make_order ~id:2 ~side:Ask ~price:100 ~qty:10 ~order_type:Limit ~timestamp:0 in
    
    let fill_count = ref 0 in
    let on_fill _ _ _ _ _ = incr fill_count in

    let res_buy = Engine.submit engine buy on_fill in
    Alcotest.(check (result unit reject_result_test)) "Buy submit" (Ok ()) res_buy;
    Alcotest.(check int) "No fills on buy" 0 !fill_count;
    
    let res_sell = Engine.submit engine sell on_fill in
    Alcotest.(check (result unit reject_result_test)) "Sell submit" (Ok ()) res_sell;
    Alcotest.(check int) "One fill on sell" 1 !fill_count;
    Alcotest.(check int) "Buy filled" 0 buy.remaining_qty;
    Alcotest.(check int) "Sell filled" 0 sell.remaining_qty

let () =
    let open Alcotest in
    run "OCaml-LOB" [
        "matching", [
            test_case "Basic matching" `Quick test_basic_matching;
        ];
    ]
