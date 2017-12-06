open Base

let () =
  let starting_bank =
    Bank.of_list [2; 8; 8; 5; 4; 2; 3; 1; 5; 5; 1; 2; 15; 13; 5; 14]
  in
  let rec loop count seen current =
    match Set.mem seen current with
    | true -> count
    | false -> loop (count + 1) (Set.add seen current) (Bank.distribute current)
  in
  loop 0 (Set.empty (module Bank)) starting_bank |> Stdio.printf "%d\n"
