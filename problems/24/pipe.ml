open Base

module T = struct
  type t = int * int [@@deriving compare, sexp_of]
end

include T
include Comparable.Make(T)

let of_string s = Caml.Scanf.sscanf s "%d/%d" (fun a b -> (a, b))
let value (a, b) = a + b
