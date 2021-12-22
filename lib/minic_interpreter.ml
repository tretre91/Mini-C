open Minic_ast

(** Exception utilisée pour sortir de l'évaluation d'une séquence d'instructions *)
exception Ret of int

(** Interprète un programme, prog est supposé avoir passé la vérification de types *)
let interpret_program (prog: prog) =
  (* Table de hachage contenant toutes les variables du programme, elle évolue au cours de
     l'execution *)
  let env = Hashtbl.create 256 in

  (* Table de hachage contenant toutes les fonctions du programme *)
  let fenv = Hashtbl.create 128 in

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

  (* Ajoute une variables à l'environnement et stocke son nom au sommet de la pile *)
  let rec add_var x e =
    let l = Stack.top variables in
    l := x :: !l;
    Hashtbl.add env x (eval_expr e)
  (* Sauvegarde les variables du sommet de la pile *)
  and push_vars () =
    Stack.push (ref []) variables
  (* Supprime de l'environnement les variables du sommet de la pile et restaure
     les variables précédentes *)
  and pop_vars () =
    let vars = Stack.pop variables in
    List.iter (Hashtbl.remove env) !vars
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
      let fn = Hashtbl.find fenv name in
      eval_function fn args
  (* Fonction d'évaluation d'un appel de fonction *)
  and eval_function (fdef: fun_def) (args: expr list): int =
    push_vars ();
    List.iter2 (fun (x, _) e -> add_var x e) fdef.params args;
    let return_value = 
      try
        eval_block fdef.body;
        0
      with
      | Ret n -> n
    in
    pop_vars ();
    return_value
  (* Fonction d'évaluation d'un bloc de code *)
  and eval_block (b: block): unit =
    push_vars ();
    try
      List.iter eval_instr b;
      pop_vars ()
    with
    | Ret n -> pop_vars (); raise (Ret n)
  (* Fonction d'évaluation d'une instruction *)
  and eval_instr (i: instr): unit = match i with
    | Putchar e -> 
      let n = eval_expr e in
      print_char (Char.chr n)
    | Decl d ->
      List.iter (fun (x, _, e) -> add_var x e) d
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
  
  (* On cherche la fonction main tout en ajoutant les variables globales et les
     autres fonctions à leurs environnements *)
  let rec exec prog =
    match prog with
    | [] ->
      print_endline "no main function!";
      0
    | Variable(x, _, e) :: tl -> add_var x e; exec tl
    | (Function f) :: tl ->
        if f.name = "main" then
          eval_function f []
        else begin
          Hashtbl.add fenv f.name f;
          exec tl
        end
  in
  push_vars ();
  exec prog
