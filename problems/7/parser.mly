%{
open Robot_record
%}

%token <int> WEIGHT
%token <string> NAME
%token LPAREN
%token RPAREN
%token ARROW
%token COMMA
%token EOF

%start <Robot_record.t list> lines
%%

lines:
| l = line*; EOF { l }

line:
| s = self_data; { let name, weight = s in
                   {name; weight; children = [] }}
| s = self_data; ARROW; children = child_list; { let name, weight = s in
                                                 {name; weight; children;}}

self_data:
| n = NAME; LPAREN; w = WEIGHT; RPAREN; { (n, w) }

child_list:
| n = NAME; { [n] }
| n = NAME; COMMA; l = child_list; { n :: l }
