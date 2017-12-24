open Base
open Stdio

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end
  include T
  include Comparable.Make(T)
  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let turn_left (x, y) = (-y, x)
  let turn_right (x, y) = (y, -x)
end

let load_line y line =
  String.to_list line
  |> List.filter_mapi ~f:(fun x elem ->
      match elem with
      | '#' -> Some (x, -y)
      | _ -> None)

let load_lines =
  List.concat_mapi ~f:load_line

let rec loop step infected position direction count =
  match step with
  | 10000 -> count
  | _ -> let current_infected = Set.mem infected position in
    let new_direction = if current_infected then Coord.turn_right direction else Coord.turn_left direction in
    let new_infected = if current_infected then Set.remove infected position else Set.add infected position in
    let new_position = Coord.add position new_direction in
    loop (step + 1) new_infected new_position new_direction (if current_infected then count else count + 1)


let () =
  let filename = Caml.Sys.argv.(1) in
  let lines = In_channel.read_lines filename in
  let dimensions = (String.length (List.hd_exn lines), List.length lines) in
  let center = (fst dimensions / 2, -(snd dimensions / 2)) in
  let infected = Set.of_list (module Coord) (load_lines lines) in
  loop 0 infected center (0, 1) 0
  |> printf "Part 1: %d\n"
