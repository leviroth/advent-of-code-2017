open Base
open Stdio

let caught ?(delay=0) (depth, range) =
  (delay + depth) % ((range - 1) * 2) = 0

let safe scanners delay =
  List.for_all scanners ~f:(Fn.non (caught ~delay))

let loop_until_good ~f =
  let rec aux n =
    match f n with
    | true -> n
    | false -> aux (n + 1)
  in aux 0

let sum = List.fold ~init:0 ~f:(+)

let () =
  let filename = Caml.Sys.argv.(1) in
  let scanners = In_channel.read_lines filename
                 |> List.map ~f:(fun s ->
                     Caml.Scanf.sscanf s "%d: %d" (fun x y -> (x, y)))
  in
  let severity = scanners
                 |> List.filter ~f:(caught)
                 |> List.map ~f:(fun (x, y) -> x * y)
                 |> sum
  in
  let least_delay = loop_until_good ~f:(safe scanners)
  in printf "Part 1: %d\nPart 2: %d\n" severity least_delay
