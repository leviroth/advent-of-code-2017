open Base
open Stdio

let difference : int list -> int =
  fun l ->
    Option.value_exn
      (Option.map2 ~f:( - )
         (List.max_elt ~cmp:Int.compare l)
         (List.min_elt ~cmp:Int.compare l))

let line_to_int_list =
  fun l -> l
           |> String.split ~on:'\t'
           |> List.map ~f:Int.of_string


let () =
  In_channel.with_file "input.txt" ~f:(fun ic ->
      In_channel.input_lines ~fix_win_eol:true ic
      |> List.map ~f:line_to_int_list
      |> List.map ~f:difference
      |> List.fold ~init:0 ~f:(+)
      |> Out_channel.printf "%d\n")
