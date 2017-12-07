open Base

type t = {name: string;
          weight: int;
          children: string list} [@@deriving sexp, fields]
