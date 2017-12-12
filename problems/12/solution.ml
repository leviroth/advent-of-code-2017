open Base
open Stdio

module Graph = struct
  type ('key, 'value, 'cmp) t = ('key, 'value, 'cmp) Map.t

  let of_alist_exn = Map.of_alist_exn

  let dfs graph ~source =
    let visited = Set.Using_comparator.empty (Map.comparator graph) in
    let rec aux graph source visited count =
      Map.find_exn graph source
      |> List.fold ~init:(visited, count) ~f:(fun (visited, count) next ->
             if Set.mem visited next then (visited, count)
             else aux graph next (Set.add visited next) (count + 1) )
    in
    aux graph source visited 0

end

let parse lexbuf = Parser.lines Lexer.read lexbuf

let process_file filename =
  In_channel.with_file filename ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      parse lexbuf )


let build_graph lines = Graph.of_alist_exn (module Int) lines

let () =
  let filename = Caml.Sys.argv.(1) in
  process_file filename |> build_graph
  |> Graph.dfs ~source:0 |> fun (_, count) -> printf "%d\n" count

