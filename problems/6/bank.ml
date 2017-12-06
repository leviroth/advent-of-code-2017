open Base
open Stdio

module T = struct
  type t = {contents: (int, int, Int.comparator_witness) Map.t; size: int}

  let compare a b = Map.compare_direct Int.compare a.contents b.contents

  let sexp_of_t {contents} =
    Map.sexp_of_m__t (module Int) sexp_of_int contents

end

include T
include Comparable.Make (T)

let of_list l =
  let rec aux l ({contents; size} as acc) =
    match l with
    | [] -> acc
    | hd :: tl -> aux tl {contents= Map.add contents size hd; size= size + 1}
  in
  aux l {contents= Map.empty (module Int); size= 0}


let to_list bank = Map.to_alist bank.contents

let get_biggest {contents} =
  let alist = Map.to_alist contents in
  Option.value_exn
    (List.max_elt alist ~cmp:(fun (_, a) (_, b) -> Int.compare a b))


let distribute bank =
  let location = fst (get_biggest bank) in
  let n = Map.find_exn bank.contents location in
  let emptied_contents = Map.add bank.contents location 0 in
  let add_to_all = n / bank.size in
  let num_extras = n % bank.size in
  let extra_indices =
    List.range (location + 1) (location + num_extras + 1)
    |> List.map ~f:(fun x -> x % bank.size)
  in
  let all_added = Map.map emptied_contents ~f:(( + ) add_to_all) in
  { bank with
    contents=
      List.fold extra_indices ~init:all_added ~f:(fun contents index ->
          Map.change contents index ~f:(Option.map ~f:(( + ) 1)) ) }

