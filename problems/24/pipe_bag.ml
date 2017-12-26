open Base

let empty = Map.empty (module Int)
let add_pipe map (a, b) =
  Map.add_multi map ~key:a ~data:b
  |> Map.add_multi ~key:b ~data:a
let of_list = List.fold ~init:empty ~f:add_pipe
let remove_pipe map (start, endpoint) =
  let map = Map.change map start ~f:(Option.map ~f:(List.filter ~f:((<>) endpoint))) in
  Map.change map endpoint ~f:(Option.map ~f:(List.filter ~f:((<>) start)))
