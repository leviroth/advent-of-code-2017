open Base

type t = Spin of int | Exchange of int * int | Partner of char * char

let of_string s =
  let rest = String.slice s 1 0 in
  let pairify s' =
    match String.split s' ~on:'/' with
    | h1 :: h2 :: tl -> (h1, h2)
    | _ -> assert false
  in
  match s.[0] with
  | 's' -> Spin (Int.of_string rest)
  | 'x' -> let a, b = pairify rest in Exchange (Int.of_string a, Int.of_string b)
  | 'p' -> let a, b = pairify rest in Partner (a.[0], b.[0])
  | _ -> assert false

let apply t arr =
  match t with
  | Spin n -> let index = (Array.length arr) - n in
    let tl = Array.slice arr index 0 in
    let hd = Array.slice arr 0 index in
    Array.append tl hd
  | Exchange (a, b) -> Array.swap arr a b; arr
  | Partner (a, b) -> let find_index c = Array.findi_exn arr ~f:(fun _ -> Char.equal c) |> fst
    in let a_index, b_index = find_index a, find_index b
    in Array.swap arr a_index b_index; arr
