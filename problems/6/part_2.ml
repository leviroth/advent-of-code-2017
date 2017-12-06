open Base

let () =
  let starting_bank =
    Bank.of_list [2; 8; 8; 5; 4; 2; 3; 1; 5; 5; 1; 2; 15; 13; 5; 14]
  in
  let rec loop count seen current =
    match Map.find seen current with
    | Some n -> count - n
    | None -> loop (count + 1) (Map.add seen current count) (Bank.distribute current)
  in
  loop 0 (Map.empty (module Bank)) starting_bank |> Stdio.printf "%d\n"
