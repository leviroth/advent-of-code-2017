open Base
open Stdio

let update_generator (current, factor) =
  let modulus = 2147483647 in
  (current * factor % modulus, factor)

let low_match a b =
  let mask = Int.pow 2 16 - 1 in
  fst a land mask = fst b land mask


let rec loop step count (generator_a, generator_b) =
  match step with
  | 40000000 -> count
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
  let generator_a = (a_seed, 16807) in
  let generator_b = (b_seed, 48271) in
  loop 0 0 (generator_a, generator_b) |> printf "%d\n"
