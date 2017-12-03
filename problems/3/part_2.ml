open Base
open Stdio

module Coordinate = struct
  module T = struct
    type t = int * int [@@deriving (sexp_of, compare)]
  end

  let add (x, y) (x', y') = (x + x', y + y')

  include T
  include Comparable.Make (T)
end

let get_from_coord map coord = coord |> Map.find map |> Option.value ~default:0

let neighbors coord =
  let directions =
    [(1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1)]
  in
  List.map ~f:(Coordinate.add coord) directions


let search n =
  let rec aux map last coord shell =
    if last > n then last
    else
      let direction =
        if snd coord = -shell then (1, 0)
        else if fst coord = -shell then (0, -1)
        else if snd coord = shell then (-1, 0)
        else (0, 1)
      in
      let next_coord = Coordinate.add coord direction in
      let next_last =
        neighbors next_coord |> List.map ~f:(get_from_coord map)
        |> List.fold ~init:0 ~f:( + )
      in
      let next_map = Map.add map ~key:next_coord ~data:next_last in
      let next_shell = if fst next_coord > shell then shell + 1 else shell in
      aux next_map next_last next_coord next_shell
  in
  let starting_map = Map.singleton (module Coordinate) (0, 0) 1 in
  aux starting_map 0 (0, 0) 0


let () = 289326 |> search |> Stdio.Out_channel.printf "%d\n"
