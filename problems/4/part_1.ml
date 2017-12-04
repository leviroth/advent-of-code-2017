open Base
open Stdio

let no_duplicates comparator l =
  let init = Set.empty comparator in
  let rec aux set l =
    match l with
    | [] -> true
    | hd :: tl -> if Set.mem set hd then false else aux (Set.add set hd) tl
  in aux init l

let valid passphrase =
  let words = String.split ~on:' ' passphrase in
  no_duplicates (module String) words

let () =
  In_channel.with_file "input.txt" ~f:(
    In_channel.fold_lines ~init:0 ~f:(fun count next_line ->
        count + if valid next_line then 1 else 0))
  |> Out_channel.printf "%d\n"
