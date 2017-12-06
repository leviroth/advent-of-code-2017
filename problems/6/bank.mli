open Base

type t

include Comparable.S with type t := t

val of_list : int list -> t

val distribute : t -> t
