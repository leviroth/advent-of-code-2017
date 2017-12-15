open Base
open Stdio

let expand_binary_string s =
  let length = String.length s in
  let padding_length = 8 - length in
  String.concat [String.make padding_length '0'; s]


let hash_string_binary s : string =
  Common.Hash.dense_hash s |> List.map ~f:Stdint.Int32.of_int
  |> List.map ~f:Stdint.Int32.to_string_bin
  |> List.map ~f:(fun s -> String.drop_prefix s 2)
  |> List.map ~f:expand_binary_string |> String.concat


let used_squares s =
  let char_to_bool = Char.equal '1' in
  let lines_to_hash =
    List.map (List.range 0 128) ~f:(Printf.sprintf "%s-%d" s)
  in
  let hash_strings = List.map lines_to_hash ~f:hash_string_binary in
  List.map hash_strings ~f:String.to_list
  |> List.map ~f:(List.map ~f:char_to_bool)


module Pair = struct
  module T = struct
    type t = int * int [@@deriving (sexp_of, compare)]
  end

  include T
  include Comparable.Make (T)
end

let graph_of_array a =
  let all_coords =
    let l = List.range 0 128 in
    List.cartesian_product l l
  in
  let get g (x, y) = g.(y).(x) in
  let neighbors g (x, y) =
    let neighbor_coords =
      let in_range n = 0 <= n && n <= 127 in
      List.filter [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] ~f:
        (fun (x', y') -> in_range x' && in_range y' )
    in
    List.filter neighbor_coords ~f:(get g)
  in
  let alist =
    List.filter all_coords ~f:(get a)
    |> List.map ~f:(fun p -> (p, neighbors a p))
  in
  Common.Graph.of_alist_exn (module Pair) alist


let () =
  let input = "ljoxqyyw" in
  let grid =
    used_squares input |> List.map ~f:Array.of_list |> Array.of_list
  in
  let square_count =
    Array.map grid ~f:(Array.count ~f:Fn.id) |> Array.fold ~init:0 ~f:( + )
  in
  let _, region_count =
    used_squares input |> List.map ~f:Array.of_list |> Array.of_list
    |> graph_of_array |> Common.Graph.partition
  in
  printf "Part 1: %d\nPart 2: %d\n" square_count region_count

