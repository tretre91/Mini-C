open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)

(* Vérification du bon typage d'un programme. *)
let typecheck_program (prog: prog) =
  (* L'environnement global mémorise le type de chaque variable globale. *)
  let global_env =
    List.fold_left (fun env (x, ty, _) -> Env.add x ty env) Env.empty prog.globals
  in

  (* Vérification du bon typage d'une fonction.
     C'est une fonction locale : on a accès à [prog] et à [global_env]. *)
  let typecheck_function (fdef: fun_def) =
    (* Lève une exception qui contient un message d'erreur indiquant le nom
       de la fonction où l'erreur a eu lieu *)
    let error message =
      failwith (Printf.sprintf "Type error in function %s: %s" fdef.name message)
    in
    
    (* L'environnement local mémorise le type des paramètres et de chaque
       variable locale. *)
    let local_env =
      let param_env =
        List.fold_left (fun env (x, ty) -> Env.add x ty env) Env.empty fdef.params
      in
      List.fold_left (fun env (x, ty) -> Env.add x ty env) param_env fdef.params
    in

    (* Récuperation du type d'une variable *)
    let type_var x =
      match Env.find_opt x local_env with
      | Some t -> t
      | None -> match Env.find_opt x global_env with
        | Some t -> t
        | None -> error (Printf.sprintf "undefined variable %s" x)
    in
    
    (* Vérification du bon typage et calcul du type d'une expression.
       À nouveau, fonction locale avec accès à tout ce qui est au-dessus. *)
    let rec type_expr = function
      | Cst _ -> Int
      | BCst _ -> Bool
      | Add(e1, e2) | Mul(e1, e2) ->
        begin match type_expr e1, type_expr e2 with
        | Int, Int -> Int
        | _, _ -> error "type error"
        end
      | Get(id) -> type_var id
      | Call(f, args) -> type_call f args
      (* À COMPLÉTER *)
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

    (* Vérification du bon typage d'une instruction ou d'une séquence.
       Toujours local. *)
    let rec typecheck_instr = function
      | Set(id, e) ->
        let t_e = type_expr e in
        let t_var = type_var id in
        if t_var <> t_e then
          error "type error"
      (* Cas d'une instruction [return]. On vérifie que le type correspond au
         type de retour attendu par la fonction dans laquelle on se trouve. *)
      | Return(e) ->
        let t = type_expr e in
        if t <> fdef.return then
          error "type error"
      | Expr(e) -> ignore (type_expr e)
      (* À COMPLÉTER *)
                   
    and typecheck_seq s =
      List.iter typecheck_instr s        
    in

    (* Code principal du typage d'une fonction : on type ses instructions. *)
    typecheck_seq (fdef.code);
  in

  (* Code principal du typage d'un programme : on type ses fonctions.
     Il faudrait aussi vérifier les valeurs initiales des variables globales.
     À COMPLÉTER
   *)
  List.iter typecheck_function (prog.functions);
