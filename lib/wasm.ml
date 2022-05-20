type dtype = I32 | I64 | F32 | F64

type qualifier = Mut | Const

type expr =
  | Instr of string list
  | Module of seq
  | Block of seq
  | Loop of seq
  | If of seq * seq
  | Function of string * dtype list * dtype option * dtype list * seq
  | Global of string * qualifier * dtype * seq
  | Start of string
  | Memory of int
  | Comment of string
and seq = expr list

let string_of_typ = function
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"

let string_of_qualifier = function
  | Mut -> "mut"
  | Const -> "const"
