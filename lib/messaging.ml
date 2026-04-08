(** {1 OCaml 5 Multicore & Lock-Free Messaging}

    Implements a High-Performance Multi-Producer Single-Consumer (MPSC)
    queue using OCaml 5 Atomics. This enables a 'Domain-per-Symbol'
    architecture where the matching engine is isolated from order entry. *)

type 'a node =
    | Nil
    | Next of 'a * 'a node Atomic.t

type 'a t = {
    head : 'a node Atomic.t;
    tail : 'a node Atomic.t;
}

let create () =
    let dummy = Next (Obj.magic (), Atomic.make Nil) in
    { head = Atomic.make dummy; tail = Atomic.make dummy }

let push q v =
    let new_node = Next (v, Atomic.make Nil) in
    let rec loop () =
        let old_tail = Atomic.get q.tail in
        match old_tail with
        | Nil -> loop () (* Should not happen *)
        | Next (_, next) ->
            if Atomic.compare_and_set next Nil new_node then
                Atomic.set q.tail new_node
            else
                loop ()
    in
    loop ()

let pop q =
    let old_head = Atomic.get q.head in
    match old_head with
    | Nil -> None
    | Next (_, next) ->
        let next_node = Atomic.get next in
        match next_node with
        | Nil -> None
        | Next (v, _) ->
            Atomic.set q.head next_node;
            Some v
