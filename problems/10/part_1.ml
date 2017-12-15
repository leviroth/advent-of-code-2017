open Base
open Stdio

let parse s =
  s |> String.rstrip |> String.split ~on:',' |> List.map ~f:Int.of_string


let () =
  let lengths = parse (In_channel.read_all "inputs/day_10.txt") in
  let n = 256 in
  let module M = Common.Hash.Rope (struct
    let size = n
  end) in
  let r = M.init () in
  let r' = List.fold ~init:r ~f:M.update lengths in
  printf "%d\n" (M.product r')

