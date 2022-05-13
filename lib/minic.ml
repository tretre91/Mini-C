(** Représentation intermédiaire similaire à l'AST, mais sans les informations
    de type et avec des modifications facilitant la traduction en représentation
    sous forme de pile *)

module Ast = Minic_ast

(* Redéfinition de certains types de l'AST *)
type typ = Ast.typ =
  | Int
  | Bool
  | Void

type binop = Ast.binop =
  | Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Leq | Gt | Geq
  | And | Or
  | BAnd | BOr | BXor
  | Lsl | Asr

type unop = Ast.unop =
  | Minus
  | Not
  | BNot

type expr = Ast.expr =
  | Cst of int
  | BCst of bool
  | UnaryOperator of unop * expr
  | BinaryOperator of binop * expr * expr
  | Get of string
  | Call of string * expr list

(** Représentation des instructions, on n'utilise plus le constructeur de déclaration
    de variable dans cette représentation *)
type instr =
  | Putchar of expr
  | Set of string * expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
  | Expr of expr
  | Block of block
(** Type des blocs de code *)
and block = instr list

type fun_def = {
  name: string;
  params: string list;
  locals: string list;
  return: typ;
  body: block;
}

type prog = {
  globals: string list;
  functions: fun_def list;
}

(** Convertit la représentation sous forme d'ast en cette représentation
    intermédiaire *)
let prog_of_ast ast =
  let env = Hashtbl.create 32 in
  (* Fonction de traduction d'une fonction *)
  let tr_fun f =
    (* on donne un id unique à chacune des variables locales, permet de différentier
       des variables redefinies dans un sous-bloc *)
    let locals_stack = Stack.create () in
    let local_names = ref [] in
    let make_id =
      let id = ref 0 in
      fun () ->
        let name = Printf.sprintf "local_%d" !id in
        incr id;
        name
    in
    let add_var v =
      let name = make_id () in
      let block_locals = Stack.top locals_stack in
      block_locals := name :: !block_locals;
      local_names := name :: !local_names;
      Hashtbl.add env v name
    in
    let get_var v = Hashtbl.find env v in
    let enter_block () =
      Stack.push (ref []) locals_stack
    in
    let exit_block () =
      let block_locals = !(Stack.pop locals_stack) in
      List.iter (Hashtbl.remove env) block_locals;
    in

    (* Fonction de traduction d'un expression, traduit les noms de variables dans
       le cas Get v *)
    let rec tr_expr = function
      | Get v -> Get (get_var v)
      | UnaryOperator (op, e) -> UnaryOperator (op, tr_expr e)
      | BinaryOperator (op, e1, e2) -> BinaryOperator (op, tr_expr e1, tr_expr e2)
      | Call (f, args) -> Call (f, List.map tr_expr args)
      | _ as e -> e
    in

    (* Fonction de traduction d'une instruction *)
    let rec tr_instr = function (* TODO : trouver autre chose que le flatten *)
      | Ast.Putchar e -> [Putchar (tr_expr e)]
      | Ast.Decl vars ->
        List.map (fun (v, _, e) -> add_var v; Set (get_var v, tr_expr e)) vars
      | Ast.Set (v, e) -> [Set (get_var v, tr_expr e)]
      | Ast.If (c, b1, b2) -> [If (tr_expr c, tr_block b1, tr_block b2)]
      | Ast.While (c, b) -> [While (tr_expr c, tr_block b)]
      | Ast.Return e -> [Return (tr_expr e)]
      | Ast.Expr e -> [Expr (tr_expr e)]
      | Ast.Block b -> [Block (tr_block b)]
    and tr_block b =
      enter_block ();
      let b' = List.flatten (List.map tr_instr b) in
      exit_block ();
      b'
    in

    let params = List.map (fun (s, _) -> Hashtbl.add env s s; s) Ast.(f.params) in
    let body = tr_block Ast.(f.body) in
    {
      name = Ast.(f.name);
      params = params;
      locals = !local_names;
      return = Ast.(f.return);
      body = body;
    }
  in

  let globals, funcs, init_instr =
    List.fold_left (fun (globals, funcs, init_instr) global ->
      match global with
      | Ast.Variable (v, _, e) -> 
        Hashtbl.add env v v; 
        v::globals, funcs, Set (v, e) :: init_instr
      | Ast.Function f -> globals, tr_fun f :: funcs, init_instr
    ) ([], [], []) ast
  in
  (* on crée une fonction init qui contient les initialisations de variables globales 
     TODO plus tard : initiliser directement ? appeler la fonction automatiquement ? *)
  let init_fun = {
    name = "__init";
    params = [];
    locals = [];
    return = Void;
    body = init_instr;
  }
  in
  { globals = globals; functions = init_fun :: funcs }