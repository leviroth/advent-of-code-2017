open Base
open Stdio

type direction = N | Nw | Ne | S | Sw | Se

let parse s =
  let dir_map =
    String_dict.of_alist_exn
      [("n", N); ("nw", Nw); ("ne", Ne); ("s", S); ("sw", Sw); ("se", Se)]
  in
  String.split s ~on:',' |> List.map ~f:(String_dict.find_exn dir_map)


let coord = function
  | N -> (0, 2)
  | Nw -> (-1, 1)
  | Ne -> (1, 1)
  | S -> (0, -2)
  | Sw -> (-1, -1)
  | Se -> (1, -1)


let add_coord (x, y) (x', y') = (x + x', y + y')

let ending_coord path =
  List.map ~f:coord path |> List.fold ~init:(0, 0) ~f:add_coord


let distance (x, y) =
  let x, y = (abs x, abs y) in
  match Ordering.of_int (compare x y) with
  | Equal -> x
  | Less -> x + (y - x) / 2
  | Greater -> x


let max_distance path =
  let rec aux path location max_so_far =
    match path with
    | [] -> max_so_far
    | hd :: tl ->
        let next_coord = add_coord location (coord hd) in
        aux tl next_coord (max max_so_far (distance next_coord))
  in
  aux path (0, 0) 0


let () =
  In_channel.read_all "input.txt" |> String.rstrip |> parse
  |> fun path ->
  printf "Part 1: %d\n" (distance (ending_coord path)) ;
  printf "Part 2: %d\n" (max_distance path)

