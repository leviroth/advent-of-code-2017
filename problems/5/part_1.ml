open Base
open Stdio


let jump arr n =
  let next = n + arr.(n) in
  arr.(n) <- arr.(n) + 1;
  next

let _ =
  let lines = In_channel.read_lines "input.txt" in
  let jumps = Array.of_list_map lines ~f:Int.of_string in
  let location = ref 0 in
  let count = ref 0 in
  while 0 <= !location && !location < Array.length jumps do
    location := jump jumps !location;
    Caml.incr count
  done;
  Out_channel.printf "%d\n" !count
