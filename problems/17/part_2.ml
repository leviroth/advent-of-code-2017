open Base
open Stdio

let compute skip max =
  let rec loop successor step position =
    if step = max then successor else
      let old_size = step in
      let position_for_insert = (position + skip) % old_size in
      let new_successor = if position_for_insert = 0 then step else successor in
      loop new_successor (step + 1) (position_for_insert + 1)
  in loop 1 2 1

let () =
  compute 343 50000001 |> printf "%d\n"
