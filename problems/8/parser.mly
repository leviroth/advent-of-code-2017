%{
open Core_kernel
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
     direction;
     amount} }

condition:
| IF; c = REGISTER; relation = relation; value = NUMBER
  { { c_register = c;
      relation;
      value} }

direction:
| INC { Increase }
| DEC { Decrease }

relation:
| LT { Lt }
| GT { Gt }
| LE { Le }
| GE { Ge }
| NE { Ne }
| EQ { Eq }
