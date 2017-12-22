type value_expr = Register of char | Number of int

type t =
  | Snd of value_expr
  | Set of char * value_expr
  | Add of char * value_expr
  | Mul of char * value_expr
  | Mod of char * value_expr
  | Rcv of char
  | Jgz of value_expr * value_expr

