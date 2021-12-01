open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)

(* Vérification du bon typage d'un programme. *)
let typecheck_program (prog: prog) =
  (* Vérification du bon typage et calcul du type d'une expression dans un
    environnement donné. *)
  let type_expr env e =
    let rec type_expr = function
      | Cst _ -> Int
      | BCst _ -> Bool
      | Add(e1, e2) | Mul(e1, e2) ->
        begin match type_expr e1, type_expr e2 with
        | Int, Int -> Int
        | _, _ -> failwith "type error"
        end
      | Lt(e1, e2) ->
        begin match type_expr e1, type_expr e2 with
        | Int, Int -> Bool
        | _, _ -> failwith "type error"
        end
      | Get(x) ->
        begin match Env.find_opt x env with
        | Some t -> t
        | None -> failwith ("undefined variable " ^ x)
        end
      | Call(f, args) -> type_call f args
    and type_call f args =
      let func =
        match List.find_opt (fun func -> func.name = f) prog.functions with
        | Some f -> f
        | None -> failwith ("call to undefined function " ^ f)
      in
      let rec typecheck_args params args =
        match params, args with
        | [], [] -> ()
        | [], _ -> failwith ("too many arguments in call to " ^ func.name)
        | _, [] -> failwith ("missing arguments in call to " ^ func.name)
        | (_, t)::params', e::args' ->
          if type_expr e <> t then
            failwith "type error"
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
      | Void, _ -> failwith "cannot declare a variable of type void"
      | _, _ ->
        if ty = te then
          Env.add x ty env
        else
          failwith "type error"
    in
    List.fold_left add_var env variables
  in

  (* L'environnement global mémorise le type de chaque variable globale. *)
  let global_env =
    typecheck_declaration_list prog.globals Env.empty
  in

  (* Vérification du bon typage d'une fonction.
     C'est une fonction locale : on a accès à [prog] et à [global_env]. *)
  let typecheck_function (fdef: fun_def) =
    (* Lève une exception qui contient un message d'erreur indiquant le nom
       de la fonction où l'erreur a eu lieu. *)
    let error message =
      failwith (Printf.sprintf "Type error in function %s: %s" fdef.name message)
    in

    (* L'environnement local contient les types de toutes les variables de la
       fonction (globales, paramètres et locales).
       Si plusieurs variables ont le même nom alors l'ordre de priorité est
          locale > paramètre > globale *)
    let local_env =
      let param_env =
        List.fold_left (fun env (x, ty) -> 
          if ty = Void then
            error (Printf.sprintf "'void %s', cannot use a parameter of type void" x)
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
            error "putchar expects an integer (ascii code) argument"
      | Set(x, e) ->
        let t_e = type_expr e in
        let t_var = match Env.find_opt x local_env with
          | Some t -> t
          | None -> error ("undefined variable " ^ x)
        in
        if t_var <> t_e then
          error "type error"
      | If(cond, t, f) ->
          if type_expr cond <> Bool then
            error "expecting a boolean expression in an if's condition"
          else begin
            typecheck_seq t;
            typecheck_seq f
          end
      | While(cond, s) ->
        if type_expr cond <> Bool then
          error "expecting a boolean expression in while loop's condition"
        else
          typecheck_seq s
      | Return(e) ->
        let t = type_expr e in
        begin match fdef.return, t with
        | Void, _ -> error "cannot return a value from a void function"
        | t1, t2 when t1 <> t2 -> error "mismatch between the returned value and the function's type"
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
      error "A non void function should return a value"
  in

  (* Code principal du typage d'un programme : on type ses fonctions.
     Il faudrait aussi vérifier les valeurs initiales des variables globales.
     À COMPLÉTER
   *)
  List.iter typecheck_function (prog.functions)

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
    | Add(e1, e2) | Mul(e1, e2) | Lt(e1, e2) ->
      check_expr e1;
      check_expr e2
    | Call _ -> failwith "Cannot initialize a global variable with a function's result"
    in
    List.iter (fun (_, _, e) -> check_expr e) vars
  in

  check_redefinition prog.globals;
  check_global_expression prog.globals;
  List.iter check_redefinition (List.map (fun fdef -> fdef.locals) prog.functions)
