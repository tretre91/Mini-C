type var =
  | Global of string
  | Param of int
  | Local of int

type num_op =
  | Add | Sub | Mul | Div | Mod
  | Eq | Eqz | Neq | Lt | Leq | Gt | Geq
  | And | Or | Not
  | BAnd | BOr | BXor
  | Lsl | Asr

type instr =
  | Cst of int
  | Op of num_op
  | Get of var
  | Set of var
  | Load of Wasm.dtype
  | Store of Wasm.dtype
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

let num_op_of_binop (op: Minic.binop) : num_op =
  match op with
  | Add  -> Add
  | Sub  -> Sub
  | Mult -> Mul
  | Div  -> Div
  | Mod  -> Mod
  | Eq   -> Eq
  | Neq  -> Neq
  | Lt   -> Lt
  | Leq  -> Leq
  | Gt   -> Gt
  | Geq  -> Geq
  | And  -> And
  | BAnd -> BAnd
  | Or   -> Or
  | BOr  -> BOr
  | BXor -> BXor
  | Lsl  -> Lsl
  | Asr  -> Asr
