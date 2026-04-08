(** {1 Real-Time Analytics Server}

    An OCaml [Dream] web server that bridges the high-performance matching
    engine with the browser-based dashboard via WebSockets. *)

open Ocaml_lob
open Types
module Engine = Engine
module Stats = Stats

(** {2 Snapshot Utilities} *)

let snapshot_to_json (engine : Engine.t) =
    let levels_to_json levels side_sort =
        PriceMap.bindings levels
        |> List.sort (fun (p1, _) (p2, _) -> side_sort p2 p1) (* descending for bids, ascending for asks *)
        |> List.map (fun (price, level) ->
            `Assoc [
                ("price", `Float (price_to_float price));
                ("size", `Int level.pl_total_qty);
                ("totalSize", `Int level.pl_total_qty); (* Should be cumulative for depth *)
                ("depth", `Int (min 100 (level.pl_total_qty / 10)))
            ])
        |> (fun l -> `List l)
    in
    `Assoc [
        ("type", `String "SNAPSHOT");
        ("asks", levels_to_json engine.book.asks.levels compare);
        ("bids", levels_to_json engine.book.bids.levels (fun a b -> compare b a));
    ]

(** {2 Server Logic} *)

let () =
    let config = default_config in
    let engine = Engine.create config in
    let stats = Stats.create 100_000 in
    
    (* Pre-fill with some liquidity for the demo *)
    let dummy_on_fill _ _ _ _ _ = () in
    let _ = Engine.submit engine (make_order ~id:1 ~side:Buy ~price:1502000 ~qty:500 ~order_type:Limit ~timestamp:0) dummy_on_fill in
    let _ = Engine.submit engine (make_order ~id:2 ~side:Ask ~price:1503000 ~qty:450 ~order_type:Limit ~timestamp:0) dummy_on_fill in

    Dream.run ~port:8080
    @@ Dream.logger
    @@ Dream.router [
        (* Static Files *)
        Dream.get "/" (Dream.from_filesystem "." "front/index.html");
        Dream.get "/app.js" (Dream.from_filesystem "." "front/app.js");

        (* WebSocket Endpoint *)
        Dream.get "/ws" (fun _ ->
            Dream.websocket (fun websocket ->
                let rec loop () =
                    let%lwt () = Lwt_unix.sleep 0.5 in (* Update UI every 500ms *)
                    
                    (* 1. Send Order Book Snapshot *)
                    let snapshot = snapshot_to_json engine |> Yojson.Safe.to_string in
                    let%lwt () = Dream.send websocket snapshot in
                    
                    (* 2. Send Performance Stats *)
                    let lat = Stats.get_percentile stats 0.99 in
                    let ops = Stats.get_throughput stats in
                    let stats_msg = `Assoc [
                        ("type", `String "STATS");
                        ("latency", `Float lat);
                        ("throughput", `Float ops)
                    ] |> Yojson.Safe.to_string in
                    let%lwt () = Dream.send websocket stats_msg in
                    
                    loop ()
                in

                (* Handle incoming messages (Manual Orders from Dashboard) *)
                let rec receiver () =
                    match%lwt Dream.receive websocket with
                    | Some msg ->
                        let json = Yojson.Safe.from_string msg in
                        (match Yojson.Safe.Util.member "type" json |> Yojson.Safe.Util.to_string with
                         | "ORDER" ->
                             let side = if Yojson.Safe.Util.member "side" json |> Yojson.Safe.Util.to_string = "BUY" then Buy else Ask in
                             let price = Yojson.Safe.Util.member "price" json |> Yojson.Safe.Util.to_float |> float_to_price in
                             let size = Yojson.Safe.Util.member "size" json |> Yojson.Safe.Util.to_int in
                             let order = make_order ~id:(Random.int 1000000) ~side ~price ~qty:size ~order_type:Limit ~timestamp:0 in
                             
                             let start = Unix.gettimeofday () in
                             let _ = Engine.submit engine order (fun passive_id active_id price qty _ ->
                                 let fill_msg = `Assoc [
                                     ("type", `String "TRADE");
                                     ("trade", `Assoc [
                                         ("side", `String (side_to_string side));
                                         ("price", `Float (price_to_float price));
                                         ("size", `Int qty);
                                         ("passive_id", `Int passive_id);
                                         ("active_id", `Int active_id)
                                     ])
                                 ] |> Yojson.Safe.to_string in
                                 Lwt.ignore_result (Dream.send websocket fill_msg)
                             ) in
                             let ns = (Unix.gettimeofday () -. start) *. 1_000_000_000.0 |> int_of_float in
                             Stats.record stats ns;
                             receiver ()
                         | _ -> receiver ())
                    | None -> Lwt.return_unit
                in

                Lwt.choose [loop (); receiver ()]));
    ]
