open Base
open Stdio

module Pipe = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end
  include T
  include Comparable.Make(T)
  let of_string s = Caml.Scanf.sscanf s "%d/%d" (fun a b -> (a, b))
  let value (a, b) = a + b
end

module Pipe_bag = struct
  let empty = Map.empty (module Int)
  let add_pipe map (a, b) =
    Map.add_multi map ~key:a ~data:b
    |> Map.add_multi ~key:b ~data:a
  let of_pipes = List.fold ~init:empty ~f:add_pipe
  let remove_pipe map (start, endpoint) =
    let map = Map.change map start ~f:(Option.map ~f:(List.filter ~f:((<>) endpoint))) in
    Map.change map endpoint ~f:(Option.map ~f:(List.filter ~f:((<>) start)))
end

let rec max pins bag =
  let endpoints = Map.find_exn bag pins in
  let candidate_pipes = List.map endpoints ~f:(fun endpoint -> (pins, endpoint)) in
  let next_step = List.map candidate_pipes ~f:(fun (start, endpoint) ->
      let bag = Pipe_bag.remove_pipe bag (start, endpoint) in
      max endpoint bag + start + endpoint) in
  List.max_elt next_step ~cmp:compare
  |> Option.value ~default:0

let () =
  let filename = Caml.Sys.argv.(1) in
  let lines = In_channel.read_lines filename in
  let pipes = List.map lines ~f:Pipe.of_string in
  let bag = Pipe_bag.of_pipes pipes in
  max 0 bag
  |> printf "%d\n"
