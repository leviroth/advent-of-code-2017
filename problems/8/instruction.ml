type direction = Increase | Decrease
type relation = Lt | Gt | Le | Ge | Ne | Eq

type action = {a_register: string;
               direction: direction;
               amount: int}

type condition = {c_register: string;
                  relation: relation;
                  value: int}

type t = {action: action;
          condition: condition}
