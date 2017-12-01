open Base
open Stdio

let () =
  let l =
    In_channel.read_all "input.txt" |> String.to_list
    |> List.filter ~f:Char.is_digit
    |> List.map ~f:(fun x -> x |> String.of_char |> Int.of_string)
  in
  let hd = List.hd_exn l in
  let rec aux hd l total =
    match l with
    | last :: [] -> if last = hd then total + last else total
    | hd1 :: (hd2 :: tl as rest) -> aux hd rest (if hd1 = hd2 then hd1 + total else total)
    | [] -> assert false
  in
  Out_channel.printf "%d\n" (aux hd l 0)

