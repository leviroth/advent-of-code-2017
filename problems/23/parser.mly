%token <int> NUMBER
%token <char> REGISTER
%token EOF
%token SET
%token SUB
%token MUL
%token JNZ

%start <Parsetree.t list> lines
%%

lines:
| l = line*; EOF { l }

line:
|  SET; x = REGISTER; y = value_expr; { Parsetree.Set (x, y) }
|  SUB; x = REGISTER; y = value_expr; { Parsetree.Sub (x, y) }
|  MUL; x = REGISTER; y = value_expr; { Parsetree.Mul (x, y) }
|  JNZ; x = value_expr; y = value_expr; { Parsetree.Jnz (x, y) }

value_expr:
| c = REGISTER; { Parsetree.Register c }
| n = NUMBER; { Parsetree.Number n }
