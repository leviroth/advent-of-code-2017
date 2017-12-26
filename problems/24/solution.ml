open Base
open Stdio

type 'a comparer = {default : 'a;
                    cmp : 'a -> 'a -> int;
                    combine : int * int -> 'a -> 'a;}

let strongest = {default = 0;
                 cmp = compare;
                 combine = fun (start, endpoint) strength ->
                   strength + start + endpoint }

let longest_then_strongest = {default = (0, 0);
                              cmp = [%compare: int *int];
                              combine = fun (start, endpoint) (length, strength) ->
                                (length + 1, strength + start + endpoint);}

let rec max comparer pins bag =
  let endpoints = Map.find_exn bag pins in
  let candidate_pipes = List.map endpoints ~f:(fun endpoint -> (pins, endpoint)) in
  let next_step = List.map candidate_pipes ~f:(fun (start, endpoint) ->
      let bag = Pipe_bag.remove_pipe bag (start, endpoint) in
      let acc = max comparer endpoint bag in
      comparer.combine (start, endpoint) acc) in
  List.max_elt next_step ~cmp:comparer.cmp
  |> Option.value ~default:comparer.default

let () =
  let filename = Caml.Sys.argv.(1) in
  let lines = In_channel.read_lines filename in
  let pipes = List.map lines ~f:Pipe.of_string in
  let bag = Pipe_bag.of_list pipes in
  let part_1 = max strongest 0 bag in
  let part_2 = max longest_then_strongest 0 bag |> snd in
  printf "Part 1: %d\nPart 2: %d\n" part_1 part_2
