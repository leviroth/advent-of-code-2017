open Base

type state = A | B | C | D | E | F

type value = Zero | One

type t = {position : int;
          state : state;
          tape : value Map.M(Int).t;}

let next state value =
  match state, value with
  | A, Zero -> (One, 1, B)
  | A, One -> (Zero, -1, B)
  | B, Zero -> (Zero, 1, C)
  | B, One -> (One, -1, B)
  | C, Zero -> (One, 1, D)
  | C, One -> (Zero, -1, A)
  | D, Zero -> (One, -1, E)
  | D, One -> (One, -1, F)
  | E, Zero -> (One, -1, A)
  | E, One -> (Zero, -1, D)
  | F, Zero -> (One, 1, A)
  | F, One -> (One, -1, E)

let empty_tape = Map.empty (module Int)

let starter = {position = 0; state = A; tape = empty_tape;}

let checksum t = Map.data t.tape |> List.count ~f:(function | One -> true | Zero -> false)

let step t =
  let value = Map.find t.tape t.position |> Option.value ~default:Zero in
  let value, direction, state = next t.state value in
  {position = t.position + direction;
   state;
   tape = Map.add t.tape t.position value;}
