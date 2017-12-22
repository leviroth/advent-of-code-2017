type triple = int * int * int
type t = { position: triple;
           velocity: triple;
           acceleration: triple;}

let manhattan (x, y, z) = (abs x) + (abs y) + (abs z)

let manhattan_compare a b =
  compare (manhattan a) (manhattan b)

let compare a b =
  match manhattan_compare a.acceleration b.acceleration with
  | 0 -> (match manhattan_compare a.velocity b.velocity with
      | 0 -> manhattan_compare a.position b.position
      | n -> n)
  | n -> n
