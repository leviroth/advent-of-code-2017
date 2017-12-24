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
  let reverse (x, y) = (-x, -y)
end

type state =
  | Clean
  | Weakened
  | Infected
  | Flagged

let load_line y line =
  String.to_list line
  |> List.filter_mapi ~f:(fun x elem ->
      match elem with
      | '#' -> Some (x, -y)
      | _ -> None)

let load_lines lines =
  List.concat_mapi lines ~f:load_line
  |> List.fold ~init:(Map.empty (module Coord)) ~f:(fun map coord ->
      Map.add map coord Infected)

let cycle_state =
  function
  | Clean -> Weakened
  | Weakened -> Infected
  | Infected -> Flagged
  | Flagged -> Clean

let rec loop step grid position direction count =
  let turn_fn =
    function
    | Clean -> Coord.turn_left
    | Weakened -> Fn.id
    | Infected -> Coord.turn_right
    | Flagged -> Coord.reverse
  in
  match step with
  | 10000000 -> count
  | _ -> let current_state = Map.find grid position |> Option.value ~default:Clean in
    let new_direction = (turn_fn current_state) direction in
    let new_infected = Map.add grid position (cycle_state current_state) in
    let new_position = Coord.add position new_direction in
    let new_count = match current_state with | Weakened -> count + 1 | _ -> count in
    loop (step + 1) new_infected new_position new_direction new_count


let () =
  let filename = Caml.Sys.argv.(1) in
  let lines = In_channel.read_lines filename in
  let dimensions = (String.length (List.hd_exn lines), List.length lines) in
  let center = (fst dimensions / 2, -(snd dimensions / 2)) in
  let grid = load_lines lines in
  loop 0 grid center (0, 1) 0
  |> printf "%d\n"
