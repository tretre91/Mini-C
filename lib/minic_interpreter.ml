open Minic_ast

(* Exception utilisée pour sortir de l'évaluation d'une séquence d'instructions *)
exception Ret of int

let interpret_program (prog: prog) =
  (* Table de hachage contenant toutes les variables du programme, elle évolue au cours de
     l'execution *)
  let env = Hashtbl.create 256 in

  (* Pile contenant les noms des variables introduites lors d'un appel de fonction
    ou à l'entrée dans un bloc de code, les variables au sommet sont celles
    qu'il faudra supprimer en sortant du bloc en cours d'éxécution *)
  let variables = Stack.create () in
  
  let int_of_bool b = if b then 1 else 0 in
  let bool_of_int i = i <> 0 in

  (* Applique une opération unaire *)
  let apply_unop (op: unop) (v: int): int =
    match op with
    | Minus -> -v
    | Not -> int_of_bool (not (bool_of_int v))
    | BNot -> lnot v
  in

  (* Applique une opération binaire *)
  let apply_binop (op: binop) (v1: int) (v2: int): int =
    match op with
    | Add  -> v1 + v2
    | Sub  -> v1 - v2
    | Mult -> v1 * v2
    | Div  -> v1 / v2
    | Mod  -> v1 mod v2
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
    | Lsl  -> v1 lsl v2
    | Asr  -> v1 asr v2
  in

  (* Ajoute une liste de variables à l'environnement et stocke leur noms au sommet de la pile *)
  let rec push_vars (vars: (string * expr) list): unit =
    let add_var (vars: string list) ((x, e): string * expr): string list =
      Hashtbl.add env x (eval_expr e);
      x :: vars
    in
    let vars = List.fold_left add_var [] vars in
    Stack.push vars variables
  (* Supprime de l'environnement les variables du sommet de la pile *)
  and pop_vars () =
    let vars = Stack.pop variables in
    List.iter (Hashtbl.remove env) vars
  (* Fonction d'évaluation d'une expression *)
  and eval_expr = function
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
      eval_function fn args
  (* Fonction d'évaluation d'un appel de fonction *)
  and eval_function (fdef: fun_def) (args: expr list): int =
    let params = List.map2 (fun (x, _) v -> x, v) fdef.params args in
    push_vars params;
    let return_value = 
      try
        eval_block fdef.body;
        0
      with
      | Ret n -> n
    in
    pop_vars ();
    return_value
  (* Fonction d'évaluation d'une séquence d'instructions *)
  and eval_seq (instructions: seq): unit =
    List.iter eval_instr instructions
  (* Fonction d'évaluation d'un bloc de code *)
  and eval_block (b: block): unit =
    push_vars (List.map (fun (x, _, e) -> x, e) b.locals);
    try
      eval_seq b.code;
      pop_vars ()
    with
    | Ret n -> pop_vars (); raise (Ret n)
  (* Fonction d'évaluation d'une instruction *)
  and eval_instr (i: instr): unit = match i with
    | Putchar e -> 
      let n = eval_expr e in
      print_char (Char.chr n)
    | Set(x, e) ->
      Hashtbl.replace env x (eval_expr e)
    | If(e, t, f) ->
      if eval_expr e = 1 then
        eval_block t
      else
        eval_block f
    | While(e, s) ->
      if eval_expr e = 1 then begin
        eval_block s;
        eval_instr i
      end
    | Return e ->
      raise (Ret (eval_expr e))
    | Expr e ->
      ignore (eval_expr e)
    | Block b ->
      eval_block b
  in
  
  (* On execute la fonction main si elle existe *)
  match List.find_opt (fun fdef -> fdef.name = "main") prog.functions with
  | None ->
    print_endline "No main function"; 0
  | Some f ->
    List.iter (fun (x, _, e) -> Hashtbl.add env x (eval_expr e)) prog.globals;
    eval_function f []
