open Base
open Stdio

let () =
  let input = In_channel.read_all "inputs/day_10.txt" |> String.rstrip in
  printf "%s\n" (Common.Hash.hash_string input)

