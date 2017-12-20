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
    let instruction = instructions.(state.Machine_1.position) in
    match Machine_1.eval state instruction with
    | Machine_1.(`Cont state) -> loop state
    | Machine_1.(`Done n) -> n
  in
  loop Machine_1.default |> printf "%d\n"

