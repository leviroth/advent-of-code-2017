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

%start <Parsetree.t list> lines
%%

lines:
| l = line*; EOF { l }

line:
|  SND; v = value_expr; { Parsetree.Snd v }
|  SET; x = REGISTER; y = value_expr; { Parsetree.Set (x, y) }
|  ADD; x = REGISTER; y = value_expr; { Parsetree.Add (x, y) }
|  MUL; x = REGISTER; y = value_expr; { Parsetree.Mul (x, y) }
|  MOD; x = REGISTER; y = value_expr; { Parsetree.Mod (x, y) }
|  RCV; x = value_expr; { Parsetree.Rcv x }
|  JGZ; x = value_expr; y = value_expr; { Parsetree.Jgz (x, y) }

value_expr:
| c = REGISTER; { Parsetree.Register c }
| n = NUMBER; { Parsetree.Number n }
