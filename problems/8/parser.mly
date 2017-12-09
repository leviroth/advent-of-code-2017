%{
open Instruction
%}

%token <int> NUMBER
%token <string> REGISTER
%token LT
%token GT
%token LE
%token GE
%token NE
%token EQ
%token IF
%token INC
%token DEC
%token EOF

%start <Instruction.t list> lines
%%

lines:
| l = line*; EOF { l }

line:
|  action = action; condition = condition
   {{ action;
      condition}}

action:
| target = REGISTER; direction = direction; amount = NUMBER;
  { {a_register = target;
     a_function = fun x -> direction x amount} }

condition:
| IF; c = REGISTER; relation = relation; value = NUMBER
  { { c_register = c;
      c_predicate = fun x -> relation x value} }

direction:
| INC { (+) }
| DEC { (-) }

relation:
| LT { (<) }
| GT { (>) }
| LE { (<=) }
| GE { (>=) }
| NE { (<>) }
| EQ { (=) }
