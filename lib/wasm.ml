type dtype = I8 | I16 | I32 | I64 | F32 | F64

type qualifier = Mut | Const

type expr =
  | Instr of string list
  | Module of seq
  | Block of seq
  | Loop of seq
  | If of seq * seq
  | Function of string * dtype list * dtype option * dtype list * seq
  | ImportedFunction of string * dtype list * dtype option
  | Global of string * qualifier * dtype * seq
  | Start of string
  | Memory of int
  | Data of int option * string
  | Comment of string
and seq = expr list

let string_of_typ = function
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"
  | _ -> failwith "other datatypes are only used internally"

let string_of_qualifier = function
  | Mut -> "mut"
  | Const -> "const"
