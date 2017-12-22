open Base
open Stdio

let parse lexbuf = Parser.particles Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )

let enumerate l =
  let rec aux l i acc =
    match l with
    | [] -> List.rev acc
    | hd :: tl -> aux tl (i + 1) ((i, hd) :: acc)
  in aux l 0 []

let () =
  let filename = Caml.Sys.argv.(1) in
  let particles = enumerate (process_file filename) in
  List.min_elt particles ~cmp:(fun (_, p1) (_, p2) -> Particle.compare p1 p2)
  |> Option.map ~f:fst
  |> Option.value ~default:(-1)
  |> printf "%d\n"
