open Base
open Stdio

let split_reverse l =
  let len = List.length l in
  List.split_n l (len / 2)


(* Putting the "fun" in functional programming. *)

let () =
  In_channel.read_all "input.txt"
  |> String.to_list |> List.filter ~f:Char.is_digit
  |> List.map ~f:(fun x -> x |> String.of_char |> Int.of_string)
  |> split_reverse
  |> fun (a, b) ->
  List.zip_exn a b |> List.filter ~f:(fun (a, b) -> a = b) |> List.unzip |> fst
  |> List.fold ~init:0 ~f:( + ) |> fun x -> x * 2 |> Out_channel.printf "%d\n"

