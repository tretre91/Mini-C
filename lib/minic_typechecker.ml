open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)

(* Vérification du bon typage d'un programme. *)
let typecheck_program (prog: prog) =
  (* Vérifie la cohérence des types d'une liste de déclaration de variables
     et ajoute ces variables à l'environnement env. *)
  let typecheck_declaration_list variables env type_exp =
    let add_var env (x, ty, e) =
      if type_exp env e = ty then
        Env.add x ty env
      else
        failwith "type error"
    in
    List.fold_left add_var env variables
  in

  (* L'environnement global mémorise le type de chaque variable globale. *)
  let global_env =
    let rec type_global env = function
      | Cst _ -> Int
      | BCst _ -> Bool
      | Add(e1, e2) | Mul(e1, e2) ->
        begin match type_global env e1, type_global env e2 with
        | Int, Int -> Int
        | _, _ -> failwith "type error"
        end
      | Lt(e1, e2) ->
        begin match type_global env e1, type_global env e2 with
        | Int, Int -> Bool
        | _, _ -> failwith "type error"
        end
      | Get x ->
        begin match Env.find_opt x env with
        | Some t -> t
        | None -> failwith ("Undefined variable " ^ x)
        end
      | Call _ -> failwith "Cannot call a function in a global variable definition"
    in
    typecheck_declaration_list prog.globals Env.empty type_global
  in

  (* Vérification du bon typage d'une fonction.
     C'est une fonction locale : on a accès à [prog] et à [global_env]. *)
  let typecheck_function (fdef: fun_def) =
    (* Lève une exception qui contient un message d'erreur indiquant le nom
       de la fonction où l'erreur a eu lieu. *)
    let error message =
      failwith (Printf.sprintf "Type error in function %s: %s" fdef.name message)
    in

    (* Récuperation du type d'une variable dans un environnement donné. *)
    let type_var env x =
      match Env.find_opt x env with
      | Some t -> t
      | None -> error (Printf.sprintf "undefined variable %s" x)
    in
    
    (* Vérification du bon typage et calcul du type d'une expression dans un
       environnement donné. *)
    let type_expr env e =
      let rec type_expr = function
        | Cst _ -> Int
        | BCst _ -> Bool
        | Add(e1, e2) | Mul(e1, e2) ->
          begin match type_expr e1, type_expr e2 with
          | Int, Int -> Int
          | _, _ -> error "type error"
          end
        | Lt(e1, e2) ->
          begin match type_expr e1, type_expr e2 with
          | Int, Int -> Bool
          | _, _ -> error "type error"
          end
        | Get(id) -> type_var env id
        | Call(f, args) -> type_call f args
      and type_call f args =
        let func =
          match List.find_opt (fun func -> func.name = f) prog.functions with
          | Some f -> f
          | None -> error (Printf.sprintf "call to undefined function %s" f)
        in
        let rec typecheck_args params args =
          match params, args with
          | [], [] -> ()
          | [], _ -> error (Printf.sprintf "too many arguments in call to %s" func.name)
          | _, [] -> error (Printf.sprintf "missing arguments in call to %s" func.name)
          | (_, t)::params', e::args' ->
            if type_expr e <> t then
              error "type error"
            else
              typecheck_args params' args'
        in
        typecheck_args func.params args;
        func.return
      in
      type_expr e
    in

    (* L'environnement local contient les types de toutes les variables de la
       fonction (globales, paramètres et locales).
       Si plusieurs variables ont le même nom alors l'ordre de priorité est
          locale > paramètre > globale *)
    let local_env =
      let param_env =
        List.fold_left (fun env (x, ty) -> Env.add x ty env) global_env fdef.params
      in
      typecheck_declaration_list fdef.locals param_env type_expr
    in

    (* A partir d'ici l'environnement n'est plus modifié, on redéfinit donc
       les fonctions auxiliaires qui prennent en argument un environnement *)

    (* Calcul du type d'une expression dans l'environnement local. *)
    let type_expr = type_expr local_env in
    (* Récuperation du type d'une variable dans l'environnement local. *)
    let type_var = type_var local_env in

    (* Vérification du bon typage d'une instruction ou d'une séquence.
       Toujours local. *)
    let rec typecheck_instr = function
      | Putchar(e) ->
          if type_expr e <> Int then
            error "putchar expects an integer (ascii code) argument"
      | Set(id, e) ->
        let t_e = type_expr e in
        let t_var = type_var id in
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
  List.iter typecheck_function (prog.functions);
