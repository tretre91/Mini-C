(** Représentation intermédiaire similaire à l'AST, mais sans les informations
    de type et avec des modifications facilitant la traduction en représentation
    sous forme de pile *)

module Ast = Minic_ast
open Minic_ast

type typ =
  | Integer of integral_type
  | Bool
  | Void
  | Ptr of typ
  | Tab of typ * int

(** Représentation des expressions, on retire les informations de type de l'AST *)
type expr =
  | Cst of const_expr
  | Cast of expr * typ * typ
  | InitList of bool * expr list
  | UnaryOperator of typ * unop * expr
  | BinaryOperator of typ * binop * expr * expr
  | Get of string
  | Read of typ * expr
  | Call of string * expr list
(** Type des valeurs connues à la compilation *)
and const_value =
  | Integral of Int64.t
  | Floating of float
  | List of const_expr list
and const_expr = {
  t: typ;
  value: const_value;
}

(** Représentation des instructions, on n'utilise plus le constructeur de déclaration
    de variable dans cette représentation *)
type instr =
  | Set of string * expr
  | StaticMemcpy of expr * int * int (* dest, id du segment, taille du segment *)
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
  static: (int * string) list; (* Données mémoire *)
  persistent: string list; (* Données persistentes, par ex : initialisateur des tableaux*)
  globals: (string * typ) list;
  functions: fun_def list;
  extern_functions: fun_def list;
}

let todo ?msg location =
  let msg = match msg with
  | None -> ""
  | Some s -> Printf.sprintf " %s." s
  in
  failwith (Printf.sprintf "TODO :%s %s" msg location)

(* Donne la taille d'un objet en octets *)
let sizeof = function
  | Integer Char -> 1
  | Integer Int | Bool | Ptr _ -> 4
  | _ -> failwith "sizeof"

let rec default_value t =
  match t with
  | Bool
  | Integer _ -> Cst { t; value = Integral Int64.zero }
  | Void -> failwith __LOC__ (* unreachable *)
  | Ptr _ -> failwith __LOC__ (* unreachable *)
  | Tab (t, n) -> InitList (true, List.init n (fun _ -> default_value t))
  
let rec make_initlist t n l =
  match l with
  | _ when n = 0 -> []
  | [] -> default_value t :: make_initlist t (n - 1) l
  | e::tl -> e :: make_initlist t (n - 1) tl

(** Calcule la valeur d'une expression constante *)
let calc_const_expr e =
  let mask t v =
    let size = 8 * sizeof t in
    let mask = Int64.sub (Int64.shift_left 1L size) 1L in
    Int64.logand v mask
  in
  let cast_const_expr e target =
    let { t; value = v } = e in
    match t, target, v with
    | Integer _, Integer _, Integral v ->
      { t = target; value = Integral (mask target v) }
    | Bool, Integer _, Integral b ->
      { t = target; value = Integral (if b = 0L then 0L else 1L) }
    | Integer _, Bool, Integral v ->
      { t = target; value = Integral (if v = 0L then 0L else 1L) }
    | _ -> failwith __LOC__
  in
  let rec calc_const_expr = function
    | Cst c -> c
    | Cast (e, _, to_) -> cast_const_expr (calc_const_expr e) to_
    | UnaryOperator (_, op, e) ->
      let e' = calc_const_expr e in
      begin match op, e'.t, e'.value with
        | Minus, Integer _, Integral v -> { e' with value = Integral (Int64.neg v) }
        | Not, Bool, Integral b   -> { e' with value = Integral (if b = 1L then 0L else 1L) }
        | BNot, Integer _, Integral v  -> { e' with value = Integral (Int64.lognot v) }
        | _, _, _ -> todo ~msg:"unop const expr" __LOC__
      end
    | BinaryOperator (_, op, e1, e2) ->
      let int64_of_bool b = if b then 1L else 0L in
      let e1' = calc_const_expr e1 in
      let e2' = calc_const_expr e2 in
      let { t = t1; value = v1 } = e1' in
      let { t = t2; value = v2 } = e2' in
      assert (t1 = t2); (* TODO : remove *)
      begin match op, v1, v2 with
        | Add, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.add i1 i2 |> mask t1) }
        | Sub, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.sub i1 i2 |> mask t1) }
        | Mult, Integral i1, Integral i2 -> { t = t1; value = Integral (Int64.mul i1 i2 |> mask t1) }
        | Div, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.div i1 i2 |> mask t1) }
        | Mod, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.rem i1 i2 |> mask t1) }
        | Eq, _, _  -> { t = Bool; value = Integral (int64_of_bool (v1 = v2)) }
        | Neq, _, _ -> { t = Bool; value = Integral (int64_of_bool (v1 <> v2)) }
        | Lt, _, _  -> { t = Bool; value = Integral (int64_of_bool (v1 < v2)) }
        | Leq, _, _ -> { t = Bool; value = Integral (int64_of_bool (v1 <= v2)) }
        | Gt, _, _  -> { t = Bool; value = Integral (int64_of_bool (v1 > v2)) }
        | Geq, _, _ -> { t = Bool; value = Integral (int64_of_bool (v1 >= v2)) }
        | And, Integral b1, Integral b2  -> { t = Bool; value = Integral (int64_of_bool (b1 = 1L && b2 = 1L)) }
        | Or, Integral b1, Integral b2   -> { t = Bool; value = Integral (int64_of_bool (b1 = 1L || b2 = 1L)) }
        | BAnd, Integral i1, Integral i2 -> { t = t1; value = Integral (Int64.logand i1 i2 |> mask t1) }
        | BOr, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.logor i1 i2 |> mask t1) }
        | BXor, Integral i1, Integral i2 -> { t = t1; value = Integral (Int64.logxor i1 i2 |> mask t1) }
        | Lsl, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.shift_left i1 (Int64.to_int i2) |> mask t1) }
        | Asr, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.shift_right i1 (Int64.to_int i2) |> mask t1) }
        | _, _, _ -> todo ~msg:"binop const expr" __LOC__
      end
    | InitList (const, l) when const ->
      (* le type n'est plus utilisé à cette étape dans le cas d'une initializer list *)
      { t = Tab(Void, -1); value = List (List.map calc_const_expr l) }
    | _ -> failwith "Expression cannot be resolved at compile time"
  in
  calc_const_expr e

(** Convertit la représentation sous forme d'ast en cette représentation
    intermédiaire *)
let prog_of_ast ast =
  let rec const_expr_of_constant = function
    | CInteger (t, v)  -> { t = Integer t; value = Integral v }
    | CBool b  -> { t = Bool; value = Integral (if b then 1L else 0L) }
    | CIList l -> { t = Tab(Void, -1); value = List (List.map const_expr_of_constant l)}
  in
  (* Crée une expression correspondant à un accès mémoire *)
  let make_ptr t base offset =
    BinaryOperator (Integer Int, Add, base, BinaryOperator (Integer Int, Mult, Cst { t = Integer Int; value = Integral (Int64.of_int (sizeof t)) }, offset))
  in
  let make_size s = Cst { t = Integer Int; value = Integral (Int64.of_int s) } in
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
  (* Initialiseurs de donneés qui seront placées en mémoire statique *)
  let persistent_data = ref [] in
  let next_data_id =
    let id = ref 0 in
    fun () ->
      let i = !id in
      incr id;
      i
  in

  let buffer_of_initlist l =
    let buffer = Buffer.create (8 * List.length l) in
    let rec insert e =
      match e.t, e.value with
      | Integer Char, Integral v -> Buffer.add_int8 buffer (Int64.to_int v)
      | (Integer Int | Bool), Integral v -> Buffer.add_int32_le buffer (Int64.to_int32 v)
      | _, List l -> List.iter insert l
      | _, _ -> todo ~msg:"buffer" __LOC__
    in
    List.iter (fun e -> insert (calc_const_expr e)) l;
    buffer
  in

  (* Fonction de traduction d'un type *)
  let rec tr_typ = function
    | Ast.Integer t -> Integer t
    | Ast.Bool -> Bool
    | Ast.Void -> Void
    | Ast.Ptr t -> Ptr (tr_typ t)
    | Tab (t, n) ->
      let n' = calc_const_expr (tr_expr n) in
      let { t = _; value = v } = n' in
      match v with
      | Integral i -> Tab (tr_typ t, Int64.to_int i)
      | _ -> failwith __LOC__ (* unreachable *)
  (* Fonction de traduction d'une expression
      - traduit les noms de variables dans le cas Get v *)
  and tr_expr e =
    match Ast.(e.expr) with
    | Ast.Cst c -> Cst (const_expr_of_constant c)
    | Ast.Cast (expr, from, to_) -> Cast (tr_expr expr, tr_typ from, tr_typ to_)
    | Ast.InitList l -> InitList (e.const, (List.map tr_expr l))
    | Ast.UnaryOperator (op, e) -> UnaryOperator (tr_typ e.t, op, tr_expr e)
    | Ast.BinaryOperator (op, e1, e2) -> BinaryOperator (tr_typ e1.t, op, tr_expr e1, tr_expr e2)
    | Ast.Get v -> Get (get_var v)
    | Ast.Call (f, args) -> Call (f, List.map tr_expr args)
    | Ast.Read (ptr, offset) ->
      let t = get_ptr_type (tr_typ Ast.(ptr.t)) in
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
      let incr_sp size = Set (sp, BinaryOperator (Integer Int, Add, Get sp, make_size size)) in
      let decr_sp size = Set (sp, BinaryOperator (Integer Int, Sub, Get sp, make_size size)) in
      (* Traduit une suite de déclaration de variables *)
      let rec tr_decl = function
        | [] -> []
        | (v, t, e) :: tl ->
          begin match tr_typ t, tr_expr e with
          | Tab (t, n), InitList (const, l) ->
            let l = make_initlist t n l in
            let size = n * (sizeof t) in
            stack_size := !stack_size + size;
            add_var v (Ptr t);
            let p = get_var v in
            let instr =
              if const then
                let buffer = buffer_of_initlist l in
                let data = Buffer.contents buffer in
                persistent_data := data :: !persistent_data;
                [StaticMemcpy (Get (get_var v), next_data_id (), String.length data)]
              else
                List.mapi (fun i e -> Write (t, make_ptr t (Get p) (make_size i), e)) l
            in
            Set (get_var v, Get sp) :: incr_sp size :: instr @ tr_decl tl
          | t, e' ->
            add_var v t;
            Set (get_var v, e') :: tr_decl tl
          end
      in
      (* Fonction de traduction d'une instruction *)
      let tr_instr = function (* TODO : trouver autre chose que le flatten *)
        | Ast.Decl vars -> tr_decl vars
        | Ast.Set (v, e) -> [Set (get_var v, tr_expr e)]
        | Ast.Write (ptr, offset, e) ->
          let t = get_ptr_type (tr_typ Ast.(ptr.t)) in
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
      params = List.map (fun (v, t) -> v, tr_typ t) Ast.(f.params);
      locals = ("__init_sp", Integer Int) :: !local_variables;
      return = tr_typ Ast.(f.return);
      body = body;
    }
  in

  let static_offset = ref 0 in
  let static, globals, funcs, extern_funcs, init_instr =
    List.fold_left (fun (static, globals, funcs, extern_funcs, init_instr) global ->
      match global with
      | Ast.Variable (v, t, e) ->
        Hashtbl.add env v v;
        let e' = tr_expr e in
        let t' = tr_typ t in
        begin match t', e' with
        | Tab (t, n), InitList (_, l) ->
          let l' = make_initlist t n l in
          let offset = !static_offset in
          let buffer = buffer_of_initlist l' in
          static_offset := offset + n * (sizeof t);
          (offset, Buffer.contents buffer) :: static, (v, Ptr t) :: globals, funcs, extern_funcs, Set (v, make_size offset)::init_instr
        | _, _ -> static, (v, t') :: globals, funcs, extern_funcs, Set (v, e') :: init_instr
        end
      | Ast.Function f -> static, globals, tr_fun f :: funcs, extern_funcs, init_instr
      | Ast.ForwardDecl f ->
        let f' = { (tr_fun f) with locals = []; body = [] } in
        static, globals, funcs, f' :: extern_funcs, init_instr
    ) ([], [], [], [], []) ast
  in
  (* on crée une fonction init qui contient les initialisations de variables globales *)
  let init_fun = {
    name = "__init";
    params = [];
    locals = [];
    return = Void;
    body = List.rev init_instr;
  }
  in
  {
    static;
    persistent = List.rev (!persistent_data);
    globals;
    extern_functions = extern_funcs;
    functions = init_fun :: funcs
  }
