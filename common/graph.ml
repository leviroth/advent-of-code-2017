open Base

type ('key, 'value, 'cmp) t = ('key, 'value, 'cmp) Map.t

let of_alist_exn = Map.of_alist_exn

let dfs graph ~source =
  let visited = Set.Using_comparator.empty (Map.comparator graph) in
  let rec aux graph source visited count =
    Map.find_exn graph source
    |> List.fold ~init:(visited, count) ~f:(fun (visited, count) next ->
            if Set.mem visited next then (visited, count)
            else aux graph next (Set.add visited next) (count + 1) )
  in
  aux graph source visited 0


let partition graph =
  let sources = Map.keys graph in
  let all_visited = Set.Using_comparator.empty (Map.comparator graph) in
  List.fold sources ~init:(all_visited, 0) ~f:
    (fun (all_visited, count) next ->
      if Set.mem all_visited next then (all_visited, count)
      else
        let new_graph, _ = dfs graph ~source:next in
        (Set.union all_visited new_graph, count + 1) )
