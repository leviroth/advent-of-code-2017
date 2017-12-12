%token <int> NUMBER
%token COMMA
%token PIPE
%token EOF

%start <(int * int list) list> lines
%%

lines:
| l = line*; EOF { l }

line:
|  start = NUMBER; PIPE; d = dests; { (start, d) }

dests:
| n = NUMBER; { [n] }
| n = NUMBER; COMMA; d = dests; { n :: d }
