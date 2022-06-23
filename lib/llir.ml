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

type constant =
  | I32Cst of Int32.t
  | I64Cst of Int64.t
  | F32Cst of float
  | F64Cst of float

type datatype =
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64

type instr =
  | Cst of constant
  | Cast of datatype * datatype
  | Op of Wasm.dtype * num_op
  | IAnd of seq * seq
  | IOr of seq * seq
  | Get of var
  | Set of var
  | Load of Wasm.dtype
  | Store of Wasm.dtype
  | MemInit of int
  | MemCpy
  | If of seq * seq
  | While of seq * seq (* Liste des instructions de la condition d'arrêt + corps de la boucle *)
  | Call of string
  | Return
  | Drop
and seq = instr list

type fun_def = {
  name: string;
  params: Wasm.dtype list;
  locals: Wasm.dtype list;
  return: Wasm.dtype option;
  code: seq;
}

type prog = {
  static: (int option * string) list;
  static_pages: int;
  globals: (string * Wasm.dtype) list;
  functions: fun_def list;
  extern_functions: (string option * fun_def) list;
}

let num_op_of_binop (op: Minic_ast.binop) : num_op =
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
