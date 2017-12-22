open Base

module Counter_queue = struct
  type 'a t = {queue: 'a Linked_queue.t; count: int ref}

  let create () = {queue= Linked_queue.create (); count= ref 0}

  let enqueue t x =
    Linked_queue.enqueue t.queue x ;
    t.count := !(t.count) + 1


  let dequeue t = Linked_queue.dequeue t.queue

  let is_empty t = Linked_queue.is_empty t.queue
end

type t =
  { registers: int Map.M(Char).t
  ; position: int
  ; in_channel: int Counter_queue.t
  ; out_channel: int Counter_queue.t }

let start p in_channel out_channel =
  let register_names = "abcdefghijklmnopqrstuvwxyz" |> String.to_list in
  let zeroes =
    List.fold register_names ~init:(Map.empty (module Char)) ~f:(fun map key ->
        Map.add map key 0 )
  in
  let registers = Map.add zeroes 'p' p in
  {registers; position= 0; in_channel; out_channel}


let two_programs () =
  let channel0, channel1 =
    (Counter_queue.create (), Counter_queue.create ())
  in
  (start 0 channel0 channel1, start 1 channel1 channel0)


let eval_value_expr state =
  let open Parsetree_2 in
  function Register c -> Map.find_exn state.registers c | Number n -> n


let advance state = {state with position= state.position + 1}

let eval state =
  let apply_fn fn x y =
    let value = eval_value_expr state y in
    { state with
      registers=
        Map.change state.registers x ~f:(Option.map ~f:(fun a -> fn a value)) }
  in
  let set state x value =
    {state with registers= Map.add state.registers x value}
  in
  let open Parsetree_2 in
  function
    | Snd e ->
        Counter_queue.enqueue state.out_channel (eval_value_expr state e) ;
        advance state
    | Set (x, y) ->
        let value = eval_value_expr state y in
        advance (set state x value)
    | Add (x, y) -> advance (apply_fn ( + ) x y)
    | Mul (x, y) -> advance (apply_fn ( * ) x y)
    | Mod (x, y) -> advance (apply_fn ( % ) x y)
    | Rcv x -> (
      match Counter_queue.dequeue state.in_channel with
      | None -> state
      | Some n -> advance (set state x n) )
    | Jgz (x, y) ->
        let xval, yval = (eval_value_expr state x, eval_value_expr state y) in
        if xval > 0 then {state with position= state.position + yval}
        else advance state

