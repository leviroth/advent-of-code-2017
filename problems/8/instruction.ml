type action = {a_register: string;
               a_function: int -> int}

type condition = {c_register: string;
                  c_predicate: int -> bool}

type t = {action: action;
          condition: condition}
