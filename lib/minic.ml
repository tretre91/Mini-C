(** Représentation intermédiaire similaire à l'AST, mais sans les informations
    de type et avec des modifications facilitant la traduction en représentation
    sous forme de pile *)

module Ast = Minic_ast

(* Redéfinition de certains types de l'AST *)
type typ = Ast.typ =
  | Int
  | Bool
  | Void
  | Ptr of typ
  | Tab of typ

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

(** Représentation des expressions, on retire les informations de type de l'AST *)
type expr =
  | Cst of int
  | BCst of bool
  | UnaryOperator of unop * expr
  | BinaryOperator of binop * expr * expr
  | Get of string
  | Read of typ * expr
  | Call of string * expr list

(** Représentation des instructions, on n'utilise plus le constructeur de déclaration
    de variable dans cette représentation *)
type instr =
  | Putchar of expr
  | Set of string * expr
  | Write of typ * expr * expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
  | Expr of expr
  | Block of block
(** Type des blocs de code *)
and block = instr list

type fun_def = {
  name: string;
  params: (string * typ) list;
  locals: (string * typ) list;
  return: typ;
  body: block;
}

type prog = {
  static: (int * int) list;
  globals: (string * typ) list;
  functions: fun_def list;
}

(** Convertit la représentation sous forme d'ast en cette représentation
    intermédiaire *)
let prog_of_ast ast =
  (* Donne la taille d'un objet en octets *)
  let sizeof = function
    | Int | Bool | Ptr _ -> 4
    | _ -> failwith "sizeof"
  in
  (* Crée une expression correspondant à un accès mémoire *)
  let make_ptr t base offset =
    BinaryOperator (Add, base, BinaryOperator (Mult, Cst (sizeof t), offset))
  in
  (* Récupère le type d'un pointeur *)
  let get_ptr_type = function
    | Ptr t -> t
    | _ -> failwith "Not a pointer"
  in
  (* environnement contenant les variables accessibles depuis un bloc ainsi que leur type *)
  let env = Hashtbl.create 32 in
  let get_var v = Hashtbl.find env v in
  (* environnement contenant les fonctions ainsi que leur type *)
  let fun_env = Hashtbl.create 32 in

  (* Fonction de traduction d'un expression
      - traduit les noms de variables dans le cas Get v *)
  let rec tr_expr e =
    match Ast.(e.expr) with
    | Ast.Cst i -> Cst i
    | Ast.BCst b -> BCst b
    | Ast.UnaryOperator (op, e) -> UnaryOperator (op, tr_expr e)
    | Ast.BinaryOperator (op, e1, e2) -> BinaryOperator (op, tr_expr e1, tr_expr e2)
    | Ast.Get v -> Get (get_var v)
    | Ast.Call (f, args) -> Call (f, List.map tr_expr args)
    | Ast.Read (ptr, offset) ->
      let t = get_ptr_type Ast.(ptr.t) in
      Read (t, make_ptr t (tr_expr ptr) (tr_expr offset))
  in

  (* Fonction de traduction d'une fonction *)
  let tr_fun f =
    Hashtbl.add fun_env Ast.(f.name) Ast.(f.return);
    (* on donne un id unique à chacune des variables locales, permet de différentier
       des variables redefinies dans un sous-bloc *)
    let locals_stack = Stack.create () in
    let local_variables = ref [] in
    let make_id =
      let id = ref 0 in
      fun () ->
        let name = Printf.sprintf "local_%d" !id in
        incr id;
        name
    in
    let add_var v t =
      let name = make_id () in
      let block_locals = Stack.top locals_stack in
      block_locals := name :: !block_locals;
      local_variables := (name, t) :: !local_variables;
      Hashtbl.add env v name
    in
    let enter_block () =
      Stack.push (ref []) locals_stack
    in
    let exit_block () =
      let block_locals = !(Stack.pop locals_stack) in
      List.iter (Hashtbl.remove env) block_locals;
    in
    
    (* Fonction de traduction d'un bloc d'instructions *)
    let rec tr_block b =
      let stack_size = ref 0 in
      let sp = "__sp" in
      let incr_sp size = Set (sp, BinaryOperator (Add, Get sp, Cst size)) in
      let decr_sp size = Set (sp, BinaryOperator (Sub, Get sp, Cst size)) in
      (* Traduit une suite de déclaration de variables *)
      let rec tr_decl = function
        | [] -> []
        | (v, t, e) :: tl ->
          begin match t, tr_expr e with
          | Tab t, Cst n ->
            let size = n * (sizeof t) in
            stack_size := !stack_size + size;
            add_var v (Ptr t);
            Set (get_var v, Get sp) :: incr_sp size :: tr_decl tl
          | t, e' ->
            add_var v t;
            Set (get_var v, e') :: tr_decl tl
          end
      in

      (* Fonction de traduction d'une instruction *)
      let tr_instr = function (* TODO : trouver autre chose que le flatten *)
        | Ast.Putchar e -> [Putchar (tr_expr e)]
        | Ast.Decl vars -> tr_decl vars
        | Ast.Set (v, e) -> [Set (get_var v, tr_expr e)]
        | Ast.Write (ptr, offset, e) ->
          let t = get_ptr_type Ast.(ptr.t) in
          [Write (t, make_ptr t (tr_expr ptr) (tr_expr offset), tr_expr e)]
        | Ast.If (c, b1, b2) -> [If (tr_expr c, tr_block b1, tr_block b2)]
        | Ast.While (c, b) -> [While (tr_expr c, tr_block b)]
        | Ast.Return e -> [Set (sp, Get "__init_sp"); Return (tr_expr e)]
        | Ast.Expr e -> [Expr (tr_expr e)]
        | Ast.Block b -> [Block (tr_block b)]
      in
      enter_block ();
      let b' = List.flatten (List.map tr_instr b) in
      exit_block ();
      b' @ [decr_sp !stack_size]
    in

    List.iter (fun (v, _) -> Hashtbl.add env v v) Ast.(f.params);
    let body = Set ("__init_sp", Get "__sp") :: tr_block Ast.(f.body) in
    {
      name = Ast.(f.name);
      params = Ast.(f.params);
      locals = ("__init_sp", Int) :: !local_variables;
      return = Ast.(f.return);
      body = body;
    }
  in

  let static_offset = ref 0 in
  let static, globals, funcs, init_instr =
    List.fold_left (fun (static, globals, funcs, init_instr) global ->
      match global with
      | Ast.Variable (v, t, e) ->
        Hashtbl.add env v v;
        let e' = tr_expr e in
        begin match t, e' with
        | Tab t, Cst n ->
          let offset = !static_offset in
          static_offset := offset + n * (sizeof t);
          (offset, n) :: static, (v, Ptr t) :: globals, funcs, Set (v, Cst offset)::init_instr
        | _, _ -> static, (v, t) :: globals, funcs, Set (v, tr_expr e) :: init_instr
        end
      | Ast.Function f -> static, globals, tr_fun f :: funcs, init_instr
    ) ([], [], [], []) ast
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
  { static = static; globals = globals; functions = init_fun :: funcs }
