open Minic_ast

(* Exception utilisée pour sortir de l'évaluation d'une séquence d'instructions *)
exception Ret of int

let interpret_program (prog: prog) =
  (* Table de hachage contenant toutes les variables, elle évolue au cours de
     l'execution *)
  let env = Hashtbl.create 256 in
  
  let int_of_bool b = if b then 1 else 0 in
  let bool_of_int i = i <> 0 in

  let apply_unop (op: unop) (v: int) =
    match op with
    | Minus -> -v
    | Not -> int_of_bool (not (bool_of_int v))
    | BNot -> lnot v
  in

  let apply_binop (op: binop) (v1: int) (v2: int) =
    match op with
    | Add  -> v1 + v2
    | Sub  -> v1 - v2
    | Mult -> v1 * v2
    | Div  -> v1 / v2
    | Eq   -> int_of_bool (v1 = v2)
    | Neq  -> int_of_bool (v1 <> v2)
    | Lt   -> int_of_bool (v1 < v2)
    | Leq  -> int_of_bool (v1 <= v2)
    | Gt   -> int_of_bool (v1 > v2)
    | Geq  -> int_of_bool (v1 >= v2)
    | And  -> int_of_bool (bool_of_int v1 && bool_of_int v2)
    | Or   -> int_of_bool (bool_of_int v1 || bool_of_int v2)
    | BAnd -> v1 land v2
    | BOr  -> v1 lor v2
    | BXor -> v1 lxor v2
  in

  let rec eval_expr = function
    | Cst n -> n
    | BCst b -> int_of_bool b
    | UnaryOperator(op, e) ->
      let v = eval_expr e in
      apply_unop op v
    | BinaryOperator(op, e1, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      apply_binop op v1 v2
    | Get x -> Hashtbl.find env x
    | Call(name, args) ->
      let fn = List.find (fun fdef -> fdef.name = name) prog.functions in
      let args' = List.map eval_expr args in
      eval_function fn args'
  and eval_function (fdef: fun_def) (args: int list): int =
    let add_var vars x v =
      Hashtbl.add env x v;
      x :: vars
    in
    let local_vars =
      let params = List.fold_left2 (fun vars (x, _) n -> add_var vars x n) [] fdef.params args in
      List.fold_left (fun vars (x, _, e) -> add_var vars x (eval_expr e)) params fdef.locals
    in
    let retval =
      try
        eval_seq fdef.code;
        0
      with
      | Ret n -> n
    in
    List.iter (Hashtbl.remove env) local_vars;
    retval
  and eval_seq (instructions: seq): unit =
    List.iter eval_instr instructions
  and eval_instr (i: instr): unit = match i with
    | Putchar e -> 
      let n = eval_expr e in
      print_char (Char.chr n)
    | Set(x, e) ->
      Hashtbl.replace env x (eval_expr e)
    | If(e, t, f) ->
      if eval_expr e = 1 then
        eval_seq t
      else
        eval_seq f
    | While(e, s) ->
      if eval_expr e = 1 then begin
        eval_seq s;
        eval_instr i
      end
    | Return e ->
      raise (Ret (eval_expr e))
    | Expr e ->
      ignore (eval_expr e)
  in
  
  (* On execute la fonction main si elle existe *)
  match List.find_opt (fun fdef -> fdef.name = "main") prog.functions with
  | None ->
    print_endline "No main function"; 0
  | Some f ->
    List.iter (fun (x, _, e) -> Hashtbl.add env x (eval_expr e)) prog.globals;
    eval_function f []
