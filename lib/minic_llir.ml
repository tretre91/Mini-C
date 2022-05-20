(** Associe à un type minic un type de données wasm *)
let dtype_of_typ = function
  | Minic.Void -> None
  | Minic.Int | Minic.Bool -> Some Wasm.I32
  | Ptr _ -> Some Wasm.I32
  | Tab _ -> failwith "unreachable"

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
  (* Traduit une expression *)
  let rec tr_expr (e:Minic.expr) next =
    match e with
    | Minic.Cst n -> Llir.Cst n :: next
    | Minic.BCst b -> Llir.Cst (if b then 1 else 0) :: next
    | Minic.Get v -> Llir.Get (convert_var v) :: next
    | Minic.Read (t, ptr) -> tr_expr ptr (Llir.Load (Option.get (dtype_of_typ t)) :: next)
    | Minic.UnaryOperator (op, e) -> begin
      match op with
      | Minic.Minus -> Llir.Cst 0 :: tr_expr e (Op Llir.Sub :: next)
      | Minic.Not -> tr_expr e (Op Llir.Not :: next)
      | Minic.BNot -> tr_expr e (Llir.Cst (-1) :: Llir.Op Llir.BXor :: next)
      end
    | Minic.BinaryOperator (op, e1, e2) ->
      tr_expr e1 (tr_expr e2 (Llir.Op (Llir.num_op_of_binop op) :: next))
    | Minic.Call (f, args) -> tr_args args (Llir.Call f :: next)
  and tr_args args next =
    List.fold_left (fun next arg -> tr_expr arg next) next args
  in
  (* Traduit une instruction *)
  let rec tr_instr i next =
    match i with
    | Minic.Putchar e -> tr_expr e (Llir.Putchar :: next)
    | Minic.Set (v, e) -> tr_expr e (Llir.Set (convert_var v) :: next)
    | Minic.Write (t, p, e) -> tr_expr p (tr_expr e (Llir.Store (Option.get (dtype_of_typ t)) :: next))
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
  let end_seq = match Minic.(func.return) with
    | Void -> [Llir.Return]
    | Int | Bool -> [Llir.Cst 0; Llir.Return]
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
let tr_prog prog = {
  Llir.static = Minic.(prog.static);
  Llir.globals = List.map (fun (v, t) -> v, Option.get (dtype_of_typ t)) Minic.(prog.globals);
  Llir.functions = List.map tr_fdef Minic.(prog.functions);
}
