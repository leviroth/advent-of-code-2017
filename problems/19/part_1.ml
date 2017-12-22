open Base
open Stdio

module Pair = struct
  module T = struct
    type t = int * int [@@deriving (compare, sexp_of)]
  end

  include T
  include Comparable.Make (T)

  let pairwise f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

  let add = pairwise ( + )

  let sub = pairwise ( - )
end

type grid = {contents: char array array; width: int; height: int}

let north = (0, -1)

let south = (0, 1)

let east = (1, 0)

let west = (-1, 0)

let directions = [north; south; east; west]

let neighbor_coords grid coord =
  List.map directions ~f:(Pair.add coord)
  |> List.filter ~f:(fun (x, y) ->
         0 <= x && x < grid.width && 0 <= y && y < grid.height )


let next_step grid coord direction =
  let neighbor_coords = neighbor_coords grid coord in
  let previous = Pair.sub coord direction in
  let neighbor_coords =
    List.filter neighbor_coords ~f:(Fn.non (Pair.equal previous))
  in
  let filled =
    List.filter neighbor_coords ~f:(fun (x, y) ->
        not (Char.equal (grid.contents).(y).(x) ' ') )
  in
  match filled with
  | [one] -> Some (one, Pair.sub one coord)
  | [_; _; _] -> Some (Pair.add coord direction, direction)
  | _ -> None


let rec loop grid coord direction acc =
  let next_acc =
    let c = (grid.contents).(snd coord).(fst coord) in
    if Char.is_alpha c then c :: acc else acc
  in
  match next_step grid coord direction with
  | Some (coord, direction) -> loop grid coord direction next_acc
  | None -> String.of_char_list (List.rev next_acc)


let load_file filename =
  let contents =
    In_channel.with_file filename ~f:In_channel.input_lines
    |> List.map ~f:String.to_list |> List.map ~f:Array.of_list |> Array.of_list
  in
  {contents; height= Array.length contents; width= Array.length contents.(0)}


let () =
  let filename = Caml.Sys.argv.(1) in
  let grid = load_file filename in
  let starting_x =
    Array.findi (grid.contents).(0) (fun _ c -> Char.equal c '|')
    |> Option.value_exn |> fst
  in
  loop grid (starting_x, 0) south [] |> printf "%s\n"

