type var =
  | Global of string
  | Param of int
  | Local of int

type instr =
  | Cst of int
  | Add
  | Mul
  | Lt
  | Get of var
  | Set of var
  | If of seq * seq
  | While of seq * seq (* Liste des instructions de la condition d'arrêt + corps de la boucle *)
  | Call of string
  | Return
  | Putchar
and seq = instr list

type fun_def = {
  name: string;
  nb_params: int;
  nb_locals: int;
  code: seq;
}

type prog = {
  globals: string list;
  functions: fun_def list;
}
