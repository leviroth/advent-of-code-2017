open Base
open Stdio

let parse lexbuf = Parser.particles Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )

let step l =
  let updated = List.map l ~f:Particle.step in
  let collisions = List.fold updated ~init:(Map.empty (module Particle.Triple)) ~f:(fun map particle ->
      Map.add_multi map particle.Particle.position particle) in
  Map.data collisions |> List.filter_map ~f:(function [x] -> Some x | _ -> None)

let () =
  let filename = Caml.Sys.argv.(1) in
  let particles = process_file filename in
  Fn.apply_n_times ~n:10000 step particles
  |> List.length
  |> printf "%d\n"
