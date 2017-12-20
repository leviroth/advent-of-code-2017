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
  | Register c -> Map.find_exn state.registers c
  | Number n -> n

let advance state = {state with position = state.position + 1}

let eval state =
  let apply_fn fn x y = let value = eval_value_expr state y in
    {state with registers = Map.update state.registers x ~f:(Option.value_map ~default:0 ~f:(fun a -> fn a value))}
  in
  let open Parsetree in
  function
  | Snd e -> `Cont (advance {state with last_sound = Some (eval_value_expr state e)})
  | Set (x, y) -> `Cont (advance {state with registers = Map.add state.registers x (eval_value_expr state y)})
  | Add (x, y) -> `Cont (advance (apply_fn (+) x y))
  | Mul (x, y) -> `Cont (advance (apply_fn ( * ) x y))
  | Mod (x, y) -> `Cont (advance (apply_fn (%) x y))
  | Rcv x -> if (eval_value_expr state x) <> 0 then `Done (Option.value_exn state.last_sound) else `Cont (advance state)
  | Jgz (x, y) -> `Cont (let xval, yval = eval_value_expr state x, eval_value_expr state y in
    if xval > 0 then {state with position = state.position + yval} else advance state)
