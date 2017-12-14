open Base
open Stdio

module type Size = sig
  val size : int
end

module Loop_array (S : Size) = struct
  type 'a t = 'a array

  let normalize n = n % S.size

  let swap a m n = Array.swap a (normalize m) (normalize n)

  let rev_subarray a start stop =
    let count = (stop - start) / 2 in
    Sequence.iter (Sequence.range 0 count) ~f:(fun n ->
        swap a (start + n) (stop - n - 1) )

end

module Rope (S : Size) = struct
  module L = Loop_array (S)

  type t = {rope: int L.t; position: int; skip: int}

  let init () = {rope= Array.init S.size ~f:Fn.id; position= 0; skip= 0}

  let update rope length =
    L.rev_subarray rope.rope rope.position (rope.position + length) ;
    { rope with
      position= rope.position + length + rope.skip; skip= rope.skip + 1 }

end

let expand_binary_string s =
  let length = String.length s in
  let padding_length = 8 - length in
  String.concat [String.make padding_length '0'; s]


let hash_string_binary s : string =
  let input_lengths = s |> String.to_list |> List.map ~f:Char.to_int in
  let lengths = List.append input_lengths [17; 31; 73; 47; 23] in
  let module M = Rope (struct
    let size = 256
  end) in
  let r = ref (M.init ()) in
  for _ = 0 to 63 do r := List.fold ~init:!r ~f:M.update lengths done ;
  let chunks =
    let start_values = List.range ~stride:16 0 256 in
    List.map start_values ~f:(fun start ->
        Array.slice !r.M.rope start (start + 16) )
  in
  let dense_hash = List.map chunks ~f:(Array.fold ~init:0 ~f:( lxor )) in
  dense_hash |> List.map ~f:Stdint.Int32.of_int
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

module Graph = struct
  type t = bool array array

  let all_coords =
    let l = List.range 0 128 in
    List.cartesian_product l l


  let get g (x, y) = g.(y).(x)

  let neighbors g (x, y) =
    let neighbor_coords =
      let in_range n = 0 <= n && n <= 127 in
      List.filter [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] ~f:
        (fun (x', y') -> in_range x' && in_range y' )
    in
    List.filter neighbor_coords ~f:(get g)


  let dfs graph ~source =
    let visited = Set.empty (module Pair) in
    let rec aux graph source visited count =
      neighbors graph source
      |> List.fold ~init:(visited, count) ~f:(fun (visited, count) next ->
             if Set.mem visited next then (visited, count)
             else aux graph next (Set.add visited next) (count + 1) )
    in
    aux graph source visited 0


  let partition graph =
    let sources = List.filter all_coords ~f:(get graph) in
    let all_visited = Set.empty (module Pair) in
    List.fold sources ~init:(all_visited, 0) ~f:
      (fun (all_visited, count) next ->
        if Set.mem all_visited next then (all_visited, count)
        else
          let new_graph, _ = dfs graph ~source:next in
          (Set.union all_visited new_graph, count + 1) )

end

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
    |> Graph.partition
  in
  printf "Part 1: %d\nPart 2: %d\n" square_count region_count

