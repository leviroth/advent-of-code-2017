open Base
open Stdio

let line_to_int_list =
  fun l -> l
           |> String.split ~on:'\t'
           |> List.map ~f:Int.of_string

let divide x y =
  if Int.rem x y = 0 then Some (x / y)
  else if Int.rem y x = 0 then Some (y / x)
  else None

let single_element_match l n =
  List.find_map ~f:(divide n) l

(* Quadratic in the length of l. Can we do better? *)

let whole_list_match l =
  let rec aux l seen =
    match l with
    | [] -> None
    | hd :: tl -> (match single_element_match seen hd with
        | Some n -> Some n
        | None -> aux tl (hd :: seen))
  in Option.value_exn (aux l [])

let () =
  In_channel.with_file "input.txt" ~f:(fun ic ->
      In_channel.input_lines ~fix_win_eol:true ic
      |> List.map ~f:line_to_int_list
      |> List.map ~f:whole_list_match
      |> List.fold ~init:0 ~f:(+)
      |> Out_channel.printf "%d\n")
