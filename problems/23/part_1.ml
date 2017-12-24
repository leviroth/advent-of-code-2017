open Base
open Stdio

let parse lexbuf = Parser.lines Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )


let () =
  let filename = Caml.Sys.argv.(1) in
  let ast = process_file filename in
  let instructions = Array.of_list ast in
  let rec loop state =
    try let instruction = instructions.(state.Machine.position)
      in loop (Machine.eval state instruction)
    with
    | Invalid_argument _ -> !(Machine.mul_count)
  in
  loop Machine.default |> printf "%d\n"

