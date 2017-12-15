open Base
open Stdio

let parse lexbuf = Parser.lines Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )


let build_graph lines = Common.Graph.of_alist_exn (module Int) lines

let () =
  let filename = Caml.Sys.argv.(1) in
  let graph = process_file filename |> build_graph in
  let _, part_1_count = Common.Graph.dfs graph ~source:0 in
  let _, part_2_count = Common.Graph.partition graph in
  printf "Part 1: %d\nPart 2: %d\n" part_1_count part_2_count

