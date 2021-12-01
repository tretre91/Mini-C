open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)


(* type de la variable, nom de la variable, type de l'expression *)
exception Bad_assignement of typ * string * typ
(* nom de l'instruction, type de l'expression *)
exception Bad_condition of string * typ
exception Undefined_variable of string
exception Void_variable of string
exception Undefined_function of string
(* nom de l'opérateur, types attendus des opérandes gauche et droits, type des expressions *)
exception Binary_operator_mismatch of string * (typ * typ) * (typ * typ)
exception Equality_operator_mismatch of typ * typ
(* nom de la fonction, nom du paramètre, type du paramètre, type de l'expression *)
exception Bad_function_arg of string * string * typ * typ
(* type de la fonction, type de l'expression renvoyée *)
exception Return_value_mismatch of typ * typ

type scope =
  | Global
  | Local of string

(* Vérification du bon typage d'un programme. *)
let typecheck_program (prog: prog) =
  let string_of_typ = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
  in

  (* Lève une exception qui contient un message d'erreur indiquant le nom
     de la fonction où l'erreur a eu lieu. *)
  let error scope exn =
    let prefix =
      match scope with
      | Global -> "global scope"
      | Local f -> "function " ^ f
    in
    let error =
      match exn with
      | Bad_assignement (tv, var, te) ->
        Printf.sprintf "cannot assign a value of type %s to variable '%s' of type %s" (string_of_typ te) var (string_of_typ tv)
      | Bad_condition (context, te) ->
        Printf.sprintf "expecting a bool expression in %s's condition, got %s" context (string_of_typ te)
      | Undefined_variable var ->
        "undefined variable " ^ var
      | Void_variable var ->
        Printf.sprintf "'void %s': cannot define a variable of type void" var
      | Undefined_function f ->
        "call to undefined function " ^ f
      | Binary_operator_mismatch (op_type, expected, got) ->
        let se1, se2 = string_of_typ (fst expected), string_of_typ (snd expected) in
        let sg1, sg2 = string_of_typ (fst got), string_of_typ (snd got) in
        Printf.sprintf "%s operator expects %s (lhs) and %s (rhs) expressions, got %s and %s" op_type se1 se2 sg1 sg2
      | Equality_operator_mismatch (t1, t2) ->
        let s1, s2 = string_of_typ t1, string_of_typ t2 in
        Printf.sprintf "equality operator expects both arguments to be of the same type, got %s and %s" s1 s2
      | Bad_function_arg (f, param, tp, te) ->
        let stp = string_of_typ tp in
        let ste = string_of_typ te in
        Printf.sprintf "in call to %s, parameter '%s' expects an argument of type %s, got %s" f param stp ste
      | Return_value_mismatch (expected, got) ->
        Printf.sprintf "cannot return a value of type %s from a %s function" (string_of_typ expected) (string_of_typ got)
      | Failure m -> m
      | e -> raise e
    in
    failwith (Printf.sprintf "Type error in %s: %s" prefix error)
  in

  (* Vérification du bon typage et calcul du type d'une expression dans un
     environnement donné. *)
  let type_expr env e =
    let rec type_expr = function
      | Cst _ -> Int
      | BCst _ -> Bool
      | ArithmeticOp(_, e1, e2) ->
        begin match type_expr e1, type_expr e2 with
        | Int, Int -> Int
        | t1, t2 -> raise (Binary_operator_mismatch ("arithmetic", (Int, Int), (t1, t2)))
        end
      | Eq(e1, e2) | Neq(e1, e2) ->
        let t1 = type_expr e1 in
        let t2 = type_expr e2 in
        if t1 = t2 then
          Bool
        else
          raise (Equality_operator_mismatch (t1, t2))
      | Lt(e1, e2) ->
        begin match type_expr e1, type_expr e2 with
        | Int, Int -> Bool
        | t1, t2 ->  raise (Binary_operator_mismatch ("comparison", (Int, Int), (t1, t2)))
        end
      | Get(x) ->
        begin match Env.find_opt x env with
        | Some t -> t
        | None -> raise (Undefined_variable x)
        end
      | Call(f, args) -> type_call f args
    and type_call f args =
      let func =
        match List.find_opt (fun func -> func.name = f) prog.functions with
        | Some f -> f
        | None -> raise (Undefined_function f)
      in
      let rec typecheck_args params args =
        match params, args with
        | [], [] -> ()
        | [], _ -> failwith ("too many arguments in call to " ^ func.name)
        | _, [] -> failwith ("missing arguments in call to " ^ func.name)
        | (p, tp)::params', e::args' ->
          let te = type_expr e in
          if te <> tp then
            raise (Bad_function_arg (func.name, p, tp, te))
          else
            typecheck_args params' args'
      in
      typecheck_args func.params args;
      func.return
    in
    type_expr e
  in

  (* Vérifie la cohérence des types d'une liste de déclaration de variables
     et ajoute ces variables à l'environnement env. *)
  let typecheck_declaration_list variables env =
    let add_var env (x, ty, e) =
      let te = type_expr env e in
      match ty, te with
      | Void, _ -> raise (Void_variable x)
      | _, _ ->
        if ty = te then
          Env.add x ty env
        else
          raise (Bad_assignement (ty, x, te))
    in
    List.fold_left add_var env variables
  in

  (* L'environnement global mémorise le type de chaque variable globale. *)
  let global_env =
    try
      typecheck_declaration_list prog.globals Env.empty
    with
    | e -> error Global e
  in

  (* Vérification du bon typage d'une fonction. *)
  let typecheck_function (fdef: fun_def) =
    (* L'environnement local contient les types de toutes les variables de la
       fonction (globales, paramètres et locales).
       Si plusieurs variables ont le même nom alors l'ordre de priorité est
          locale > paramètre > globale *)
    let local_env =
      let param_env =
        List.fold_left (fun env (x, ty) -> 
          if ty = Void then
            raise (Void_variable x)
          else
            Env.add x ty env
        ) global_env fdef.params
      in
      typecheck_declaration_list fdef.locals param_env
    in

    (* A partir d'ici l'environnement n'est plus modifié, on redéfinit donc
       les fonctions auxiliaires qui prennent en argument un environnement *)

    (* Calcul du type d'une expression dans l'environnement local. *)
    let type_expr = type_expr local_env in

    (* Vérification du bon typage d'une instruction ou d'une séquence.
       Toujours local. *)
    let rec typecheck_instr = function
      | Putchar(e) ->
          if type_expr e <> Int then
            failwith "putchar expects an integer (ascii code) argument"
      | Set(x, e) ->
        let t_e = type_expr e in
        let t_var = match Env.find_opt x local_env with
          | Some t -> t
          | None ->raise (Undefined_variable x)
        in
        if t_var <> t_e then
          raise (Bad_assignement (t_var, x, t_e))
      | If(cond, t, f) ->
          let t_c = type_expr cond in
          if t_c <> Bool then
            raise (Bad_condition ("an if", t_c))
          else begin
            typecheck_seq t;
            typecheck_seq f
          end
      | While(cond, s) ->
        let t_c = type_expr cond in
        if t_c <> Bool then
          raise (Bad_condition ("a while loop", t_c))
        else
          typecheck_seq s
      | Return(e) ->
        let t = type_expr e in
        begin match fdef.return, t with
        | Void, _ -> failwith "cannot return a value from a void function"
        | t1, t2 when t1 <> t2 -> raise (Return_value_mismatch (t1, t2))
        | _, _ -> ()
        end
      | Expr(e) -> ignore (type_expr e)
    and typecheck_seq s =
        List.iter typecheck_instr s
    in

    (* Indique si la fonction contient au moins une instruction return *)
    let rec returns = function
      | [] -> false
      | e :: tl -> match e with
        | Return _ -> true
        | If(_, t, f) -> returns t || returns f || returns tl
        | While(_, s) -> returns s || returns tl
        | _ -> returns tl
    in

    (* Code principal du typage d'une fonction : on type ses instructions. *)
    typecheck_seq (fdef.code);
    
    (* On vérifie que la fonction renvoie bien une valeur *)
    if fdef.return <> Void && not (returns fdef.code) then
      failwith "a non void function should return a value"
  in

  (* Code principal du typage d'un programme : on type ses fonctions. *) 
  List.iter (fun fdef ->
    try
      typecheck_function fdef
    with
    | e -> error (Local fdef.name) e
  ) (prog.functions)



(* Vérifications supplémentaires non liées au typage
   - redéfinition d'une variable dans un même portée
   - utilisation d'un appel de fonction dans l'expression associée à une
     variable globale *)
let strict_check (prog: prog) =
  let check_redefinition vars =
    let rec aux prev vars = 
      match vars with
      | [] -> ()
      | (x, _, _)::tl ->
        if x = prev then
          failwith ("redefinition of variable " ^ x)
        else
          aux x tl
    in
    aux "" (List.sort (fun (x, _, _) (y, _, _) -> compare x y) vars)
  in

  let check_global_expression vars =
    let rec check_expr = function
    | Cst _ | BCst _ | Get _ -> ()
    | ArithmeticOp(_, e1, e2) | Lt(e1, e2) ->
      check_expr e1;
      check_expr e2
    | Call _ -> failwith "Cannot initialize a global variable with a function's result"
    in
    List.iter (fun (_, _, e) -> check_expr e) vars
  in

  check_redefinition prog.globals;
  check_global_expression prog.globals;
  List.iter check_redefinition (List.map (fun fdef -> fdef.locals) prog.functions)
