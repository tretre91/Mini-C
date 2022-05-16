(* Traduction d'une définition de fonction *)
let tr_fdef func =
  (* Associe une portée (globale, locale, ou paramètre) à une variable *)
  let convert_var v =
    let get_i_opt x l =
      let rec scan i = function
      | [] -> None
      | y::l -> if x = y then Some i else scan (i+1) l
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
    | Minic.Get v -> Llir.Get (convert_var v) :: next
    | Minic.BinaryOperator (op, e1, e2) ->
      let llir_op = match op with
        | Minic.Add -> Llir.Add
        | Minic.Sub -> Llir.Sub
        | Minic.Mult -> Llir.Mul
        | Minic.Lt -> Llir.Lt
        | _ -> failwith "TODO"
      in
      tr_expr e1 (tr_expr e2 (llir_op :: next))
    | Minic.Call (f, args) -> tr_args args (Llir.Call f :: next)
    | _ -> failwith "TODO"
  and tr_args args next =
    List.fold_left (fun next arg -> tr_expr arg next) next args
  in
  (* Traduit une instruction *)
  let rec tr_instr i next =
    match i with
    | Minic.Putchar e -> tr_expr e (Llir.Putchar :: next)
    | Minic.Set (v, e) -> tr_expr e (Llir.Set (convert_var v) :: next)
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
  let code = tr_block Minic.(func.body) [Llir.Cst 0; Llir.Return] in
  {
    Llir.name = Minic.(func.name);
    Llir.nb_params = Minic.(List.length func.params);
    Llir.nb_locals = Minic.(List.length func.locals);
    Llir.code = code;
  }

(* Traduction d'un programme de la 1ère représentation intermédiaire à la seconde *)
let tr_prog prog = {
  Llir.globals = Minic.(prog.globals);
  Llir.functions = List.map tr_fdef Minic.(prog.functions);
}
