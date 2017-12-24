open Base
open Stdio

let arr_of_string s = String.split ~on:'/' s |> Array.of_list_map ~f:(Fn.compose Array.of_list String.to_list)

let string_of_arr arr =
  Array.map arr ~f:(Fn.compose String.of_char_list Array.to_list)
  |> Array.to_list
  |> String.concat ~sep:"/"

let split grid =
  let dim = Array.length grid in
  let stride = if dim % 2 = 0 then 2 else 3 in
  let rows_divided =
    List.range ~stride 0 dim
    |> Array.of_list
    |> Array.map ~f:(fun n -> Array.slice grid n (n + stride))
  in
  Array.map rows_divided ~f:(fun rows ->
      List.range ~stride 0 dim
      |> Array.of_list
      |> Array.map ~f:(fun start ->
      Array.map rows ~f:(fun row -> Array.slice row start (start + stride))))

let join grid =
  let join_single_row squares =
    let square_dim = Array.length squares.(0) in
    List.range 0 square_dim
    |> Array.of_list
    |> Array.map ~f:(fun row -> Array.concat_map squares ~f:(fun square -> square.(row)))
  in
  Array.concat_map grid ~f:join_single_row

let expand_array rules arr =
  arr |> string_of_arr |> Map.find_exn rules |> arr_of_string

let full_array rules arr =
  let expanded = split arr |> Array.map ~f:(Array.map ~f:(expand_array rules)) in
  join expanded


let variations_n grid mappers =
  grid :: List.map mappers ~f:(fun arr ->
      Array.map arr ~f:(fun row ->
          Array.map row ~f:(fun (x, y) -> grid.(x).(y))))

let variations_2 grid =
  let mappers =
    [[|[|(1, 0); (0, 0)|];
       [|(1, 1); (0, 1)|]|];

     [|[|(0, 1); (0, 0)|];
       [|(1, 1); (1, 0)|]|];

     [|[|(1, 0); (1, 1)|];
       [|(0, 0); (0, 1)|]|];

     [|[|(0, 1); (1, 1)|];
       [|(0, 0); (1, 0)|]|];

     [|[|(1, 1); (1, 0)|];
       [|(0, 1); (0, 0)|]|]]
  in
  variations_n grid mappers

let variations_3 grid =
  let mappers =
    [[|[|(2, 0); (1, 0); (0, 0)|];
       [|(2, 1); (1, 1); (0, 1)|];
       [|(2, 2); (1, 2); (0, 2)|]|];

     [|[|(2, 2); (2, 1); (2, 0)|];
       [|(1, 2); (1, 1); (1, 0)|];
       [|(0, 2); (0, 1); (0, 0)|]|];

     [|[|(0, 2); (1, 2); (2, 2)|];
       [|(0, 1); (1, 1); (2, 1)|];
       [|(0, 0); (1, 0); (2, 0)|]|];

     [|[|(0, 2); (0, 1); (0, 0)|];
       [|(1, 2); (1, 1); (1, 0)|];
       [|(2, 2); (2, 1); (2, 0)|]|];

     [|[|(2, 0); (2, 1); (2, 2)|];
       [|(1, 0); (1, 1); (1, 2)|];
       [|(0, 0); (0, 1); (0, 2)|]|];

     [|[|(0, 0); (1, 0); (2, 0)|];
       [|(0, 1); (1, 1); (2, 1)|];
       [|(0, 2); (1, 2); (2, 2)|]|];

     [|[|(2, 2); (1, 2); (0, 2)|];
       [|(2, 1); (1, 1); (0, 1)|];
       [|(2, 0); (1, 0); (0, 0)|]|]]
  in
  variations_n grid mappers

let variations grid =
  match Array.length grid.(0) % 2 with
  | 0 -> variations_2 grid
  | _ -> variations_3 grid


let process_line s =
  let left, right = Caml.Scanf.sscanf s "%s => %s" (fun a b -> (a, b)) in
  left
  |> arr_of_string |> variations
  |> List.map ~f:(fun variation -> (string_of_arr variation, right))

let build_dict lines =
  List.concat_map lines ~f:process_line
  |> Map.of_alist_reduce (module String) ~f:(fun a _ -> a)

let () =
  let filename = Caml.Sys.argv.(1) in
  let lines = In_channel.read_lines filename in
  let dict = build_dict lines in
  let starter = arr_of_string ".#./..#/###" in
  let after_5 = Fn.apply_n_times ~n:5 (full_array dict) starter in
  let after_18 = Fn.apply_n_times ~n:13 (full_array dict) after_5 in
  let count = Array.fold ~init:0 ~f:(
      fun sum arr -> sum + Array.count arr ~f:(Char.equal '#'))
  in
  printf "Part 1: %d\nPart 2: %d\n" (count after_5) (count after_18)
