module Ast = Minic_ast

(** Associe à un type minic un type de données wasm *)
let dtype_of_typ = function
  | Minic.Void -> None
  (* | Ast.Integer Long -> Some Wasm.I64 *)
  | Minic.Integer _ | Minic.Bool -> Some Wasm.I32
  | Minic.Ptr _ -> Some Wasm.I32
  | Minic.Tab _ -> failwith "unreachable"

(** Associe à un type minic un type de données wasm (prend en compte les i8 et I16)
    utilisé dans le cas des instructions mémoire par exemple *)
let extended_dtype_of_typ = function
  | Minic.Void -> None
  (* | Ast.Integer Long -> Some Wasm.I64 *)
  | Minic.Integer Ast.Char -> Some Wasm.I8
  | Minic.Integer Ast.Int | Minic.Bool -> Some Wasm.I32
  | Minic.Ptr _ -> Some Wasm.I32
  | Minic.Tab _ -> failwith "unreachable"

(* Traduction d'une définition de fonction *)
let tr_fdef func =
  (* Associe une portée (globale, locale, ou paramètre) à une variable *)
  let convert_var v =
    let get_i_opt x l =
      let rec scan i = function
      | [] -> None
      | (y, _)::l -> if x = y then Some i else scan (i+1) l
      in
      scan 0 l
    in
    match get_i_opt v Minic.(func.locals) with
    | Some i -> Llir.Local i
    | None ->
      match get_i_opt v Minic.(func.params) with
      | Some i -> Llir.Param i
      | None -> Global v
  in
  let constant_of_const_expr c =
    let get_integer c =
      match Minic.(c.value) with
      | Minic.Integral v -> v
      | _ -> failwith "not an integer"
    in
    match Minic.(c.t) with
    | Integer (Char | Int) | Bool -> Llir.I32Cst (Int64.to_int32 (get_integer c))
    | _ -> failwith __LOC__
  in
  let datatype_of_typ : (Minic.typ -> Llir.datatype) = function
    | Integer Char -> Int8
    | Integer Int | Bool -> Int32
    | _ -> failwith "TODO"
  in
  let make_op t op =
    let dt = Option.get (extended_dtype_of_typ t) in
    Llir.Op (dt, op)
  in
  (* Traduit une expression *)
  let rec tr_expr e next =
    match e with
    | Minic.Cst c -> Llir.Cst (constant_of_const_expr c) :: next
    | Minic.Cast (e, from, to_) -> tr_expr e (Llir.Cast (datatype_of_typ from, datatype_of_typ to_) :: next)
    | Minic.InitList (_, _) -> failwith "unreachable"
    | Minic.Get v -> Llir.Get (convert_var v) :: next
    | Minic.Read (t, ptr) -> tr_expr ptr (Llir.Load (Option.get (extended_dtype_of_typ t)) :: next)
    | Minic.UnaryOperator (t, op, e) -> begin
      match op with
      | Ast.Minus ->
        let zero = match t with
        (* | Integer Long -> Llir.I64Cst Int64.zero *)
        | Integer _ -> Llir.I32Cst Int32.zero
        | _ -> failwith __LOC__
        in
        Llir.Cst zero :: tr_expr e (make_op t Llir.Sub :: next)
      | Ast.Not -> tr_expr e (Op (I32, Llir.Not) :: next)
      | Ast.BNot ->
        let minus_one = match t with
        (* | Integer Long -> Llir.I64Cst Int64.minus_one *)
        | Integer _ -> Llir.I32Cst Int32.minus_one
        | _ -> failwith __LOC__
        in
        tr_expr e (Llir.Cst minus_one :: make_op t Llir.BXor :: next)
      end
    | Minic.BinaryOperator (t, op, e1, e2) ->
      tr_expr e1 (tr_expr e2 (make_op t (Llir.num_op_of_binop op) :: next))
    | Minic.Call (f, args) -> tr_args args (Llir.Call f :: next)
  and tr_args args next =
    List.fold_left (fun next arg -> tr_expr arg next) next args
  in
  (* Traduit une instruction *)
  let rec tr_instr i next =
    match i with
    | Minic.Set (v, e) -> tr_expr e (Llir.Set (convert_var v) :: next)
    | Minic.Write (t, p, e) -> tr_expr p (tr_expr e (Llir.Store (Option.get (extended_dtype_of_typ t)) :: next))
    | Minic.StaticMemcpy (dest, id, len) ->
      let offset = Llir.Cst (Llir.I32Cst 0l) in
      let length = Llir.Cst (Llir.I32Cst (Int32.of_int len)) in
      tr_expr dest (offset :: length :: Llir.MemInit id :: next)
    | Minic.Expr e -> tr_expr e next
    | Minic.Return e -> tr_expr e [Llir.Return]
    | Minic.If (e, b1, b2) -> 
      let s1 = tr_block b1 [] in
      let s2 = tr_block b2 [] in
      tr_expr e (Llir.If (s1, s2) :: next)
    | Minic.While (cond, b) ->
      let exit_condition = tr_expr cond [] in
      Llir.While (exit_condition, tr_block b []) :: next
    | Minic.Block b -> tr_block b next
  (* Traduit un bloc d'instructions *)
  and tr_block b next =
    match b with
    | [] -> next
    | i::tl -> tr_instr i (tr_block tl next)
  in
  let end_seq = match Minic.(func.return) with (* TODO : retirer dans les cas non-void *)
    | Void -> [Llir.Return]
    (* | Integer Long -> [Llir.Cst (Llir.I64Cst 0L); Llir.Return] *)
    | Integer _ | Bool -> [Llir.Cst (Llir.I32Cst 0l); Llir.Return]
    | _ -> []
  in
  let code = tr_block Minic.(func.body) end_seq in
  {
    Llir.name = Minic.(func.name);
    Llir.params = List.map (fun (_, t) -> Option.get (dtype_of_typ t)) Minic.(func.params);
    Llir.locals = List.map (fun (_, t) -> Option.get (dtype_of_typ t)) Minic.(func.locals);
    Llir.return = dtype_of_typ Minic.(func.return);
    Llir.code = code;
  }

(* Traduction d'un programme de la 1ère représentation intermédiaire à la seconde *)
let tr_prog prog = 
  let static =
      List.map (fun data -> None, data) Minic.(prog.persistent)
    @ List.map (fun (addr, data) -> Some addr, data) Minic.(prog.static)
  in
  {
    Llir.static = static;
    Llir.globals = List.map (fun (v, t) -> v, Option.get (dtype_of_typ t)) Minic.(prog.globals);
    Llir.functions = List.map tr_fdef Minic.(prog.functions);
    Llir.extern_functions = List.map (fun f -> { (tr_fdef f) with code = [] }) Minic.(prog.extern_functions);
  }
