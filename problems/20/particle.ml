open Base

module Triple = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp_of]
  end
  include T
  include Comparable.Make(T)

  let add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
end

type t = { position: Triple.t;
           velocity: Triple.t;
           acceleration: Triple.t;}

let manhattan (x, y, z) = (abs x) + (abs y) + (abs z)

let manhattan_compare a b =
  compare (manhattan a) (manhattan b)

let compare a b =
  match manhattan_compare a.acceleration b.acceleration with
  | 0 -> (match manhattan_compare a.velocity b.velocity with
      | 0 -> manhattan_compare a.position b.position
      | n -> n)
  | n -> n

let step p =
  let new_velocity = Triple.add p.velocity p.acceleration in
  {p with position = Triple.add p.position new_velocity;
          velocity = new_velocity}
