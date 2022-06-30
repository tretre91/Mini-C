(** Représentation intermédiaire similaire à l'AST, mais sans les informations
    de type et avec des modifications facilitant la traduction en représentation
    sous forme de pile *)

module Ast = Minic_ast
open Minic_ast

type typ =
  | Integer of integral_type
  | Float
  | Double
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
  | Memcpy of expr * expr * int (* dest, src, taille *)
  | StaticMemcpy of expr * int * int (* dest, id du segment, taille du segment *)
  | Write of typ * expr * expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
  | Expr of expr
  | Block of block
(** Type des blocs de code *)
and block = instr list

(** Définition de fonction contenant seulement les informations nécessaires *)
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
  static_pages: int;
  globals: (string * typ) list;
  functions: fun_def list;
  extern_functions: (string option * fun_def) list;
}

(** Représentation des instructions contenant plus  d'informations, elle est utilisée
    dans les phases de transformation intermédiares *)
type rich_instr =
  | Set of typ * string * expr
  | Memcpy of expr * expr * int (* dest, src, taille *)
  | StaticMemcpy of expr * int * int (* dest, id du segment, taille du segment *)
  | Write of typ * expr * expr
  | If of expr * rich_block * rich_block
  | While of expr * rich_block
  | Return of (string * typ) list * expr
  | Expr of expr
  | Block of rich_block
(** Blocs de code associés à la liste des variables qui y sont déclarées *)
and rich_block = (string, typ) Hashtbl.t * rich_instr list

type rich_fun_def = {
  name: string;
  params: (string * typ) list;
  locals: (string * typ) list;
  return: typ;
  rich_body: rich_block;
  attributes: string list;
}

type rich_prog = {
  static: (int * string) list; (* Données mémoire *)
  persistent: string list; (* Données persistentes, par ex : initialisateur des tableaux*)
  static_pages: int;
  globals: (string * typ) list;
  rich_functions: rich_fun_def list;
  extern_functions: (string option * rich_fun_def) list;
}

let todo ?msg location =
  let msg = match msg with
  | None -> ""
  | Some s -> Printf.sprintf " %s." s
  in
  failwith (Printf.sprintf "TODO :%s %s" msg location)

(** [escape_string s] renvoie la chaîne s avec tous ses caractères sous format
    hexadécimal compatible avec WebAssembly ('\xx')
    par exemple : [escape_string "Bonjour" = "\\42\\6f\\6e\\6a\\6f\\75\\72"] *)
let escape_string s =
  let b = Buffer.create (3 * String.length s) in
  String.iter (fun c -> Printf.bprintf b "\\%02x" (Char.code c)) s;
  Buffer.contents b

(** [escape_non_printable s] renvoie la chaîne de caractères s avec tous ses
    caractères non imprimables correctement échappés *)
let escape_non_printable s =
  let b = Buffer.create (3 * String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string b "\\\""
    | ' '..'~' -> Buffer.add_char b c
    | _ -> Printf.bprintf b "\\%02x" (Char.code c)
  ) s;
  Buffer.contents b

(** Récupère la valeur de l'attribut correspondant à l'espace de nom d'une fonction 
    déclarée 'extern' *)
let rec get_namespace attributes =
  match attributes with
  | [] -> None
  | attr::tl ->
    try
      Scanf.sscanf attr "import::%s" (fun name -> Some name)
    with
      Scanf.Scan_failure _ -> get_namespace tl

(** Donne la taille d'un objet en octets *)
let sizeof = function
  | Integer Char -> 1
  | Integer Short -> 2
  | Integer Int | Float | Bool | Ptr _ -> 4
  | Integer Long | Double -> 8
  | _ -> failwith "sizeof"

let rec default_value t =
  match t with
  | Bool
  | Integer _ -> Cst { t; value = Integral Int64.zero }
  | Float | Double -> Cst { t; value = Floating 0.0 }
  | Void -> failwith __LOC__ (* unreachable *)
  | Ptr _ -> Cst { t; value = Integral Int64.zero }
  | Tab (t, n) -> InitList (true, List.init n (fun _ -> default_value t))
  
let rec make_initlist t n l =
  match l with
  | _ when n = 0 -> []
  | [] -> default_value t :: make_initlist t (n - 1) l
  | e::tl -> e :: make_initlist t (n - 1) tl

let get_static_string address =
  let exception Break of string in
  try
    Hashtbl.iter (fun s addr -> if addr = address then raise (Break s)) Minic_lexer.strings;
    raise Not_found
  with
    Break s -> s

(** Calcule la valeur d'une expression constante *)
let calc_const_expr e =
  let mask t v =
    let bit_size = 8 * sizeof t in
    let mask = match bit_size with
    | 64 -> Int64.minus_one
    | size -> Int64.sub (Int64.shift_left 1L size) 1L
    in
    Int64.logand v mask
  in
  let simple_precision f = Int32.float_of_bits (Int32.bits_of_float f) in
  let cast_const_expr e target =
    let { t; value = v } = e in
    let v = match t, target, v with
      | Integer _, Integer _, Integral v -> Integral (mask target v)
      | Integer _, Float,     Integral v -> Floating (Int64.to_float v)
      | Integer _, Double,    Integral v -> Floating (Int64.to_float v)
      | Integer _, Bool,      Integral v -> Integral (if v = 0L then 0L else 1L)
      | Float,     Integer _, Floating f -> Integral (mask target (Int64.of_float f))
      | Float,     Double,    Floating f -> Floating f
      | Float,     Bool,      Floating f -> Integral (if f = 0.0 then 0L else 1L)
      | Double,    Integer _, Floating f -> Integral (mask target (Int64.of_float f))
      | Double,    Float,     Floating f -> Floating (simple_precision f)
      | Double,    Bool,      Floating f -> Integral (if f = 0.0 then 0L else 1L)
      | Bool,      Integer _, Integral b -> Integral (if b = 0L then 0L else 1L)
      | Bool,      Float,     Integral b -> Floating (if b = 0L then 0.0 else 1.0)
      | Bool,      Double,    Integral b -> Floating (if b = 0L then 0.0 else 1.0)
      | _ -> failwith __LOC__
    in
    { t = target; value = v }
  in
  let rec calc_const_expr = function
    | Cst c -> c
    | Cast (e, _, to_) -> cast_const_expr (calc_const_expr e) to_
    | UnaryOperator (_, op, e) ->
      let e' = calc_const_expr e in
      begin match op, e'.t, e'.value with
        | Minus, Integer _, Integral v -> { e' with value = Integral (Int64.neg v) }
        | Minus, (Float | Double), Floating f -> { e' with value = Floating (Float.neg f) }
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
      let e' = match op, v1, v2 with
        | Add, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.add i1 i2 |> mask t1) }
        | Add, Floating f1, Floating f2  -> { t = t1; value = Floating (f1 +. f2) }
        | Sub, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.sub i1 i2 |> mask t1) }
        | Sub, Floating f1, Floating f2  -> { t = t1; value = Floating (f1 -. f2) }
        | Mult, Integral i1, Integral i2 -> { t = t1; value = Integral (Int64.mul i1 i2 |> mask t1) }
        | Mult, Floating f1, Floating f2 -> { t = t1; value = Floating (f1 *. f2) }
        | Div, Integral i1, Integral i2  -> { t = t1; value = Integral (Int64.div i1 i2 |> mask t1) }
        | Div, Floating f1, Floating f2  -> { t = t1; value = Floating (f1 /. f2) }
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
      in
      begin match e'.t, e'.value with
      | Float, Floating f -> { t = Float; value = Floating (simple_precision f) }
      | _, _ -> e'
      end
    | InitList (const, l) when const ->
      (* le type n'est plus utilisé à cette étape dans le cas d'une initializer list *)
      { t = Tab(Void, -1); value = List (List.map calc_const_expr l) }
    | _ -> failwith "Expression cannot be resolved at compile time"
  in
  calc_const_expr e

(* Conversion from rich prog to regular prog *)

let rec block_of_rich_block b =
  List.map instr_of_rich_instr (snd b)
and instr_of_rich_instr (i: rich_instr): instr =
  match i with
  | Set (_, v, e) -> Set (v, e)
  | Memcpy (dest, src, size) -> Memcpy (dest, src, size)
  | StaticMemcpy (dest, id, size) -> StaticMemcpy (dest, id, size)
  | Write (t, ptr, e) -> Write (t, ptr, e)
  | If (cond, t, f) -> If (cond, block_of_rich_block t, block_of_rich_block f)
  | While (cond, b) -> While (cond, block_of_rich_block b)
  | Return (_, e) -> Return e
  | Expr e -> Expr e
  | Block b -> Block (block_of_rich_block b)

let fun_of_rich_fun fdef =
  {
    name = fdef.name;
    params = fdef.params;
    locals = fdef.locals;
    return = fdef.return;
    body = block_of_rich_block fdef.rich_body;
  }

let prog_of_rich_prog prog =
  {
    static = prog.static;
    persistent = prog.persistent;
    static_pages = prog.static_pages;
    globals = prog.globals;
    functions = List.map fun_of_rich_fun prog.rich_functions;
    extern_functions = List.map (fun (nmsp, f) -> nmsp, fun_of_rich_fun f) prog.extern_functions;
  }

let add_reference_counting prog =
  let tr_fun f =
    let rec decr_vars vars next =
      match vars with
      | [] -> next
      | (var, Ptr _) :: tl ->
        Expr (Call ("__decrement_ref", [Get var])) :: decr_vars tl next
      | _ :: tl ->
        decr_vars tl next
    in
    let rec tr_block b =
      let local_vars, block = b in
      let decr_pointers v t acc =
        match t with
        | Ptr _ -> Expr (Call ("__decrement_ref", [Get v])) :: acc
        | _ -> acc
      in
      let b = List.flatten (List.map tr_instr block) in
      let tail = Hashtbl.fold decr_pointers local_vars [] in
      local_vars, b @ tail
    and tr_instr i =
      match i with
      | Set (Ptr _, var, _) ->
        [Expr (Call ("__decrement_ref", [Get var])); i; Expr (Call ("__increment_ref", [Get var]))]
      | Write (Ptr _ as t, ptr, _) ->
        [Expr (Call ("__decrement_ref", [Read (t, ptr)])); i; Expr (Call ("__increment_ref", [Read (t, ptr)]))]
      | If (cond, t, f) -> [If (cond, tr_block t, tr_block f)]
      | While (cond, b) -> [While (cond, tr_block b)]
      | Block b -> [Block (tr_block b)]
      | Return (vars, e) -> decr_vars vars [Return (vars, e)]
      | _ -> [i]
    in
    let body' = tr_block f.rich_body in
    { f with rich_body = body' }
  in
  let functions' = List.map (fun f ->
    if List.mem "gc_exclude" f.attributes
      then f
      else tr_fun f
  ) prog.rich_functions
  in
  { prog with rich_functions = functions' } 


(** Convertit la représentation sous forme d'ast en cette représentation
    intermédiaire *)
let prog_of_ast ast =
  let rec const_expr_of_constant = function
    | CInteger (t, v)  -> { t = Integer t; value = Integral v }
    | CFloat f  -> { t = Float; value = Floating f }
    | CDouble d -> { t = Double; value = Floating d }
    | CBool b   -> { t = Bool; value = Integral (if b then 1L else 0L) }
    | CString p -> { t = Ptr (Integer Char); value = Integral (Int64.of_int p) }
    | CIList l  -> { t = Tab(Void, -1); value = List (List.map const_expr_of_constant l)}
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
      | Integer Short, Integral v -> Buffer.add_int16_le buffer (Int64.to_int v)
      | (Integer Int | Bool | Ptr _), Integral v -> Buffer.add_int32_le buffer (Int64.to_int32 v)
      | Integer Long, Integral v -> Buffer.add_int64_le buffer v
      | Float, Floating f -> Buffer.add_int32_le buffer (Int32.bits_of_float f)
      | Double, Floating f -> Buffer.add_int64_le buffer (Int64.bits_of_float f)
      | _, List l -> List.iter insert l
      | _, _ -> todo ~msg:"buffer" __LOC__
    in
    List.iter (fun e -> insert (calc_const_expr e)) l;
    buffer
  in

  (* Fonction de traduction d'un type *)
  let rec tr_typ typ =
    let t = match typ with
      | Ast.Mut t -> t
      | Ast.Const t -> t
    in
    match t with
    | Integer t -> Integer t
    | Float -> Float
    | Double -> Double
    | Bool -> Bool
    | Void -> Void
    | Ptr t -> Ptr (tr_typ t)
    | Tab (t, n) ->
      let n' = calc_const_expr (tr_expr n) in
      let { t = _; value = v } = n' in
      match v with
      | Integral i -> Tab (tr_typ t, Int64.to_int i)
      | _ -> failwith __LOC__ (* unreachable *)
  (* Fonction de traduction d'une expression *)
  and tr_expr e =
    match Ast.(e.expr) with
    | Ast.Cst c -> Cst (const_expr_of_constant c)
    | Ast.Cast (expr, from, to_) -> Cast (tr_expr expr, tr_typ from, tr_typ to_)
    | Ast.InitList l -> InitList (e.const, (List.map tr_expr l))
    | Ast.UnaryOperator (op, e) -> UnaryOperator (tr_typ e.t, op, tr_expr e)
    | Ast.BinaryOperator (Add | Sub as op, e1, e2) ->
      begin match tr_typ e1.t with
      | Ptr t when t <> Void ->
        let offset = BinaryOperator (Integer Int, Mult, make_size (sizeof t), tr_expr e2) in
        BinaryOperator (Ptr t, op, tr_expr e1, offset)
      | _ as t -> BinaryOperator (t, op, tr_expr e1, tr_expr e2)
      end
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
      block_locals := (v, name, t) :: !block_locals;
      local_variables := (name, t) :: !local_variables;
      Hashtbl.add env v name
    in
    let get_declared_vars s =
      Stack.fold (fun acc l ->
        acc @ List.map (fun (_, name, t) -> name, t) !l
      ) [] s
    in
    let enter_block () =
      Stack.push (ref []) locals_stack
    in
    let exit_block () =
      let block_locals = !(Stack.pop locals_stack) in
      let local_env = Hashtbl.create (List.length block_locals) in
      List.iter (fun (var_name, var_id, t) ->
        Hashtbl.remove env var_name;
        Hashtbl.add local_env var_id t
      ) block_locals;
      local_env
    in
    
    (* Fonction de traduction d'un bloc d'instructions *)
    let rec tr_block b =
      let stack_size = ref 0 in
      let sp = "__sp" in
      let incr_sp size = Set (Integer Int, sp, BinaryOperator (Integer Int, Add, Get sp, make_size size)) in
      let decr_sp size = Set (Integer Int, sp, BinaryOperator (Integer Int, Sub, Get sp, make_size size)) in
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
            let instr =
              if const then
                let buffer = buffer_of_initlist l in
                let data = escape_string (Buffer.contents buffer) in
                persistent_data := data :: !persistent_data;
                [StaticMemcpy (Get (get_var v), next_data_id (), Buffer.length buffer)]
              else
                List.mapi (fun i e -> Write (t, make_ptr t (Get (get_var v)) (make_size i), e)) l
            in
            Set (Integer Int, get_var v, Get sp) :: incr_sp size :: instr @ tr_decl tl
          | Tab (Integer Char as t, n), str ->
            stack_size := !stack_size + n;
            add_var v (Ptr t);
            let memcpy = Memcpy (Get (get_var v), str, n) in
            Set (Integer Int, get_var v, Get sp) :: incr_sp n :: memcpy :: tr_decl tl
          | t, e' ->
            add_var v t;
            Set (t, get_var v, e') :: tr_decl tl
          end
      in
      (* Fonction de traduction d'une instruction *)
      let tr_instr = function (* TODO : trouver autre chose que le flatten *)
        | Ast.Decl vars -> tr_decl vars
        | Ast.Set (v, e) -> [Set (tr_typ Ast.(e.t), get_var v, tr_expr e)]
        | Ast.Write (ptr, offset, e) ->
          let t = get_ptr_type (tr_typ Ast.(ptr.t)) in
          [Write (t, make_ptr t (tr_expr ptr) (tr_expr offset), tr_expr e)]
        | Ast.If (c, b1, b2) -> [If (tr_expr c, tr_block b1, tr_block b2)]
        | Ast.While (c, b) -> [While (tr_expr c, tr_block b)]
        | Ast.Return e -> [Set (Integer Int, sp, Get "__init_sp"); Return (get_declared_vars locals_stack, tr_expr e)]
        | Ast.Expr e -> [Expr (tr_expr e)]
        | Ast.Block b -> [Block (tr_block b)]
      in
      enter_block ();
      let b' = List.flatten (List.map tr_instr b) in
      let local_env = exit_block () in
      local_env, b' @ [decr_sp !stack_size]
    in

    List.iter (fun (v, _) -> Hashtbl.add env v v) Ast.(f.params);
    let env, body = tr_block Ast.(f.body) in
    let body = env, Set (Integer Int, "__init_sp", Get "__sp") :: body in
    {
      name = Ast.(f.name);
      params = List.map (fun (v, t) -> v, tr_typ t) Ast.(f.params);
      locals = ("__init_sp", Integer Int) :: !local_variables;
      return = tr_typ Ast.(f.return);
      rich_body = body;
      attributes = Ast.(f.attributes);
    }
  in

  let static_strings =
       Hashtbl.to_seq Minic_lexer.strings
    |> Seq.map (fun (str, addr) -> addr, escape_non_printable str)
    |> List.of_seq
  in
  let static_offset = ref !Minic_lexer.strings_offset in

  (* On itère sur toutes les déclarations globales, on récupère 
     - les variables globales
     - les instructions servant à les initialiser 
     - les définitions de fonctions
     - les fonctions déclarées extern
     - les données qui devront être placées en mémoire statique *)
  let static, globals, funcs, extern_funcs, init_instr =
    List.fold_left (fun (static, globals, funcs, extern_funcs, init_instr) global ->
      match global with
      (* Cas des variables globales *)
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
          (offset, escape_string (Buffer.contents buffer)) :: static, (v, Ptr t) :: globals, funcs, extern_funcs, Set (Integer Int, v, make_size offset)::init_instr
        | _, _ -> static, (v, t') :: globals, funcs, extern_funcs, Set (t', v, e') :: init_instr
        end
      (* Cas des fonctions déclarées "extern" *)
      | Ast.Function f when f.is_forward_decl && List.mem "extern" f.attributes ->
        let f' = { (tr_fun f) with locals = []; rich_body = (Hashtbl.create 0, []) } in
        let namespace = get_namespace f.attributes in
        static, globals, funcs, (namespace, f') :: extern_funcs, init_instr
      (* cas des fonctions définies *)
      | Ast.Function f when not f.is_forward_decl ->
        static, globals, tr_fun f :: funcs, extern_funcs, init_instr
      (* Cas des prédeclarations non extern, éliminées à l'étape précedente *)
      | Ast.Function _ -> failwith __LOC__ (* unreachable *)
    ) (static_strings, [], [], [], []) ast
  in

  (* on crée une fonction init qui contient les initialisations de variables globales *)
  let init_fun = {
    name = "__init";
    params = [];
    locals = [];
    return = Void;
    body = block_of_rich_block (Hashtbl.create 0, init_instr);
  }
  in
  (* on calcule le nombre de pages utilisées par la section de données statiques *)
  let page_size = Minic_wasm.page_size in
  let stack_size = Minic_wasm.stack_size in
  let static_pages = (!static_offset / page_size) + (if !static_offset mod page_size <> 0 then 1 else 0) in
  (* on ajoute les instructions nécessaire à l'initialisation de la bibliothèque malloc si stdlib.h est inclus *)
  let init_fun =
    if Preprocessor.defined "__STDLIB_H" then
      let heap_start = Cst {
        t=Integer Int;
        value = Integral (Int64.of_int (page_size * (static_pages + stack_size)))
      }
      in
      let malloc_init: instr = Expr (Call ("__malloc_h_init", [heap_start])) in
      { init_fun with body = List.rev (malloc_init :: init_fun.body) }
    else
      init_fun
  in
  let maybe_transform cond f arg =
    if cond
    then f arg
    else arg
  in
  (* On applique d'éventuelles transformations à notre programme *)
  let prog = 
    {
      static;
      persistent = List.rev (!persistent_data);
      static_pages;
      globals;
      extern_functions = extern_funcs;
      rich_functions = funcs
    }
    |> maybe_transform (Preprocessor.defined "__MINIC_GC") add_reference_counting
    |> prog_of_rich_prog
  in
  { prog with functions = init_fun :: prog.functions }
