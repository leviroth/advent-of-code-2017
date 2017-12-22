%token <int> NUMBER
%token POSITION
%token VELOCITY
%token ACCELERATION
%token EQUALS
%token LBRACKET
%token RBRACKET
%token COMMA
%token EOF

%start <Particle.t list> particles
%%

particles:
| l = particle*; EOF { l }

particle:
|  p = position; COMMA; v = velocity; COMMA; a = acceleration; { Particle.({position = p; velocity = v; acceleration = a})  }

position:
| POSITION; EQUALS; t = triple; { t }

velocity:
| VELOCITY; EQUALS; t = triple; { t }

acceleration:
| ACCELERATION; EQUALS; t = triple; { t }

triple:
| LBRACKET; n1 = NUMBER; COMMA; n2 = NUMBER; COMMA; n3 = NUMBER; RBRACKET; { (n1, n2, n3) }
