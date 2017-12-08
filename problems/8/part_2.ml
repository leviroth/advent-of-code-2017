open Base
open Stdio

let parse lexbuf = Parser.lines Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )

module State = struct
  type t = int Map.M(String).t * int

  let condition_of_expr =
    let open Instruction in
    function
    | Lt -> (<)
    | Gt -> (>)
    | Le -> (<=)
    | Ge -> (>=)
    | Ne -> (<>)
    | Eq -> (=)

  let operation_of_expr =
    let open Instruction in
    function
    | Increase -> (+)
    | Decrease -> (-)

  let apply_action (state, greatest) action : t =
    let open Instruction in
    let operation = operation_of_expr action.direction in
    let f x = operation x action.amount in
    let new_value = Map.find state action.a_register |> Option.value ~default:0 |> f in
    let new_greatest = max greatest new_value in
    (Map.add state action.a_register new_value, new_greatest)


  let apply_instruction (state, greatest) instruction =
    let open Instruction in
    let lhs = Map.find state instruction.condition.c_register |> Option.value ~default:0 in
    if condition_of_expr instruction.condition.relation lhs instruction.condition.value
    then apply_action (state, greatest) instruction.action
    else (state, greatest)
end

let () =
  let instructions = process_file "input.txt" in
  let starting_state = (Map.empty (module String), 0) in
  let _, greatest = List.fold instructions ~init:starting_state ~f:State.apply_instruction in
  printf "%d\n" greatest
