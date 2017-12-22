%token <int> NUMBER
%token <char> REGISTER
%token EOF
%token SND
%token SET
%token ADD
%token MUL
%token MOD
%token RCV
%token JGZ

%start <Parsetree_2.t list> lines
%%

lines:
| l = line*; EOF { l }

line:
|  SND; v = value_expr; { Parsetree_2.Snd v }
|  SET; x = REGISTER; y = value_expr; { Parsetree_2.Set (x, y) }
|  ADD; x = REGISTER; y = value_expr; { Parsetree_2.Add (x, y) }
|  MUL; x = REGISTER; y = value_expr; { Parsetree_2.Mul (x, y) }
|  MOD; x = REGISTER; y = value_expr; { Parsetree_2.Mod (x, y) }
|  RCV; x = REGISTER; { Parsetree_2.Rcv x }
|  JGZ; x = value_expr; y = value_expr; { Parsetree_2.Jgz (x, y) }

value_expr:
| c = REGISTER; { Parsetree_2.Register c }
| n = NUMBER; { Parsetree_2.Number n }
