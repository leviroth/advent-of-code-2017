open Base
open Stdio

let update_generator (current, factor, criterion) =
  let modulus = 2147483647 in
  let rec aux current =
    let new_value = current * factor % modulus in
    if new_value % criterion = 0 then new_value else aux new_value
  in
  (aux current, factor, criterion)

let low_match (a, _, _) (b, _, _) =
  let mask = Int.pow 2 16 - 1 in
  a land mask = b land mask

let rec loop step count (generator_a, generator_b) =
  match step with
  | 5000000 -> count
  | _ ->
      let generator_a, generator_b =
        (update_generator generator_a, update_generator generator_b)
      in
      loop (step + 1)
        ( if low_match generator_a generator_b then count + 1
          else count )
        (generator_a, generator_b)


let () =
  let a_seed = Int.of_string Caml.Sys.argv.(1) in
  let b_seed = Int.of_string Caml.Sys.argv.(2) in
  let generator_a = (a_seed, 16807, 4) in
  let generator_b = (b_seed, 48271, 8) in
  loop 0 0 (generator_a, generator_b) |> printf "%d\n"
