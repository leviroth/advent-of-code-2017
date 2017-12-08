open Base
open Stdio

let parse lexbuf = Parser.lines Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )

module State = struct
  type t = int Map.M(String).t

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

  let apply_action state action : t =
    let open Instruction in
    let operation = operation_of_expr action.direction in
    let f = function
      | Some x -> operation x action.amount
      | None -> operation 0 action.amount
    in
    Map.update state action.a_register ~f


  let apply_instruction state instruction =
    let open Instruction in
    let lhs = Map.find state instruction.condition.c_register |> Option.value ~default:0 in
    if condition_of_expr instruction.condition.relation lhs instruction.condition.value
    then apply_action state instruction.action
    else state
end

let () =
  let instructions = process_file "input.txt" in
  let starting_state = Map.empty (module String) in
  let final_state = List.fold instructions ~init:starting_state ~f:State.apply_instruction in
  Map.data final_state |> List.max_elt ~cmp:Int.compare |> Option.value_exn |> printf "%d\n"
