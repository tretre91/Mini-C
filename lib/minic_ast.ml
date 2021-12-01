(* Représentation des types. *)
type typ =
  | Int
  | Bool
  | Void

(* Représentation des expressions. *)
type expr =
  | Cst of int
  | BCst of bool
  | ArithmeticOp of (int -> int -> int) * expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | Get of string
  | Call of string * expr list

(* Représentation des instructions et séquences. *)
type instr =
  | Putchar of expr
  | Set of string * expr
  | If  of expr * seq * seq
  | While of expr * seq
  | Return of expr
  | Expr of expr
and seq = instr list

(* Représentétion des fonctions. *)
type fun_def = {
  name: string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ * expr) list;
  code: seq;
}

(* Représentation des programmes.
   On associe une expression à chaque variable globale, cette expression
   ne peut pas contenir d'appel de fonction ou de références à des variables
   définies après la variable à laquelle elle est associée *)
type prog = {
  globals: (string * typ * expr) list;
  functions: fun_def list;
}
