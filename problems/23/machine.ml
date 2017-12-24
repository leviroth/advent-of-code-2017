open Base

type t = {registers : int Map.M(Char).t;
          position : int;
          last_sound : int option;}

let default = {registers = Map.empty (module Char);
               position = 0;
               last_sound = None;}

let eval_value_expr state =
  let open Parsetree in
  function
  | Register c -> Map.find state.registers c |> Option.value ~default:0
  | Number n -> n

let advance state = {state with position = state.position + 1}

let mul_count = ref 0

let eval state =
  let apply_fn fn x y = let value = eval_value_expr state y in
    {state with registers = Map.update state.registers x ~f:(Option.value_map ~default:0 ~f:(fun a -> fn a value))}
  in
  let open Parsetree in
  function
  | Set (x, y) -> advance {state with registers = Map.add state.registers x (eval_value_expr state y)}
  | Sub (x, y) -> advance (apply_fn ( - ) x y)
  | Mul (x, y) -> mul_count := !mul_count + 1; advance (apply_fn ( * ) x y)
  | Jnz (x, y) -> let xval, yval = eval_value_expr state x, eval_value_expr state y in
    if xval <> 0 then {state with position = state.position + yval} else advance state
