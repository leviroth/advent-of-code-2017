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

  let apply_action (state, greatest) action : t =
    let open Instruction in
    let new_value = Map.find state action.a_register |> Option.value ~default:0 |> action.a_function in
    let new_greatest = max greatest new_value in
    (Map.add state action.a_register new_value, new_greatest)


  let apply_instruction (state, greatest) instruction =
    let open Instruction in
    let lhs = Map.find state instruction.condition.c_register |> Option.value ~default:0 in
    if instruction.condition.c_predicate lhs
    then apply_action (state, greatest) instruction.action
    else (state, greatest)
end

let () =
  let instructions = process_file "input.txt" in
  let starting_state = (Map.empty (module String), 0) in
  let final_state, greatest = List.fold instructions ~init:starting_state ~f:State.apply_instruction in
  let max_in_final_state = Map.data final_state |> List.max_elt ~cmp:compare |> Option.value ~default:0 in
  printf "Part 1: %d\n" max_in_final_state;
  printf "Part 2: %d\n" greatest
