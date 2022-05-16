type var =
  | Global of string
  | Param of int
  | Local of int

type instr =
  | Cst of int
  | Add
  | Sub
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
  params: Wasm.dtype list;
  locals: Wasm.dtype list;
  return: Wasm.dtype option;
  code: seq;
}

type prog = {
  globals: (string * Wasm.dtype) list;
  functions: fun_def list;
}
