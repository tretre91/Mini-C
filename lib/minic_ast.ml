(** Représentation des types. *)
type typ =
  | Int
  | Bool
  | Void
  | Ptr of typ

(** Types des opérations binaires *)
type binop =
  | Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Leq | Gt | Geq
  | And | Or
  | BAnd | BOr | BXor
  | Lsl | Asr

(** Types des opérations unaires *)
type unop =
  | Minus
  | Not
  | BNot

(** Représentation des expressions. *)
type expr_s =
  | Cst of int
  | BCst of bool
  | UnaryOperator of unop * expr
  | BinaryOperator of binop * expr * expr
  | Get of string
  | Read of expr
  | Call of string * expr list
(* and expr = Exp of typ * expr_s *)
and expr = {
  t: typ;
  expr: expr_s
}

(** Type d'une déclaration de variable *)
type var_decl = string * typ * expr

(** Représentation des instructions. *)
type instr =
  | Putchar of expr
  | Decl of var_decl list
  | Set of string * expr
  | Write of expr * expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
  | Expr of expr
  | Block of block
(** Type des blocs de code *)
and block = instr list

(** Représentation des fonctions. *)
type fun_def = {
  name: string;
  params: (string * typ) list;
  return: typ;
  body: block;
}

(** Type d'un déclaration dans la portée globale (une déclaration de variable
    globale ou de fonction) *)
type global_decl =
  | Variable of var_decl
  | Function of fun_def

(** Représentation des programmes.
    Un programme est simplement une suite de déclarations de variables ou de fonctions. *)
type prog = global_decl list
