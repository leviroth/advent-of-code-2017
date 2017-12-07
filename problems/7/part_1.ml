open Base
open Stdio

let parse lexbuf = Parser.lines Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )


let () =
  let robot_records = process_file "input.txt" in
  let all_names =
    Set.of_list
      (module String)
      (List.map robot_records ~f:Robot_record.name)
  in
  let child_names =
    Set.of_list
      (module String)
      (List.concat_map robot_records ~f:Robot_record.children)
  in
  let difference = Set.diff all_names child_names in
  Set.choose_exn difference |> Stdio.printf "%s\n"

