type value_expr = Register of char | Number of int

type t =
  | Set of char * value_expr
  | Sub of char * value_expr
  | Mul of char * value_expr
  | Jnz of value_expr * value_expr

