open Base
open Stdio

let no_duplicates comparator l =
  let init = Set.empty comparator in
  let rec aux set l =
    match l with
    | [] -> true
    | hd :: tl -> if Set.mem set hd then false else aux (Set.add set hd) tl
  in aux init l

let print_result valid =
  "input.txt"
  |> In_channel.read_lines
  |> List.filter ~f:valid
  |> List.length
  |> Out_channel.printf "%d\n"
