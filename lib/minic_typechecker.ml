open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)

(** Exception levée lorsque l'on tente d'assigner à une variable une expression
    du mauvais type *)
exception Bad_assignement of typ * string * typ

(** Exception levée lorsqu'une condition n'a pas le type bool *)
exception Bad_condition of string * typ

(** Exception levée lors de l'accés à une variable inexistante *)
exception Undefined_variable of string

(** Exception levée lorsque l'on essaie de déclarer une variable de type void *)
exception Void_variable of string

(** Exception levée lors d'un appel à une fonction non définie *)
exception Undefined_function of string

(** Exception levée lorsqu'un opérateur unaire est appliqué au mauvais type *)
exception Unary_operator_mismatch of unop * typ

(** Exception levée lorsqu'un opérateur binaire est appliqué aux mauvais types *)
exception Binary_operator_mismatch of binop * typ * typ

(** Exception levée lorqu'un argument de fonction ne correspond pas au type du paramètre *)
exception Bad_function_arg of string * string * typ * typ

(** Exception levée lorsqu'une valeur de retour ne correspond pas au type de retour de la fonction *)
exception Return_value_mismatch of typ * typ

(** Exception levée lorsque la valeur d'une expression ne correspond pas à la valeur de l'initializer
    list qui la contient *)
exception Initializer_list_mismatch of typ * typ

(** Exception levée lorsque la valeur donnée à une variable globale n'est pas une constante *)
exception Not_const_initializer of string

(** Type indiquant où l'on se trouve dans un programme *)
type scope =
  | Global
  | Local of string

(** Lève une exception qui contient un message d'erreur indiquant le nom de la
    fonction où l'erreur a eu lieu. *)
let error =
  let rec string_of_typ = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | Ptr t -> string_of_typ t ^ " ptr"
    | Tab (t, n) -> Printf.sprintf "%s[%d]" (string_of_typ t) n
  in
  fun scope exn ->
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
      | Unary_operator_mismatch (op, t) ->
        let st = string_of_typ t in
        begin match op with
        | Minus -> "unary minus expects an integer expression, got " ^ st
        | Not   -> "boolean not excpects an expression of type bool, got " ^ st
        | BNot  -> "bitwise not expects an integer, got " ^ st
        end
      | Binary_operator_mismatch (op, t1, t2) ->
        let st1 = string_of_typ t1 in
        let st2 = string_of_typ t2 in
        let op_type, expected = match op with
        | Add | Sub | Mult | Div | Mod -> "an arithmetic", "of type int"
        | Lt | Leq | Gt | Geq          -> "a comparison", "of type int"
        | And | Or                     -> "a logical", "of type bool"
        | Eq | Neq                     -> "an equality", "of the same type"
        | BAnd | BOr | BXor            -> "a bitwise", "of type int"
        | Lsl | Asr                    -> "a shift", "of type int"
        in
        Printf.sprintf "%s operator expects its arguments to be %s, got %s and %s" op_type expected st1 st2
      | Bad_function_arg (f, param, tp, te) ->
        let stp = string_of_typ tp in
        let ste = string_of_typ te in
        Printf.sprintf "in call to %s, parameter '%s' expects an argument of type %s, got %s" f param stp ste
      | Return_value_mismatch (expected, got) ->
        Printf.sprintf "cannot return a value of type %s from a %s function" (string_of_typ expected) (string_of_typ got)
      | Initializer_list_mismatch (expr_t, list_t) ->
        Printf.sprintf "cannot use a value of type %s in an initializer list of type %s" (string_of_typ expr_t) (string_of_typ list_t)
      | Not_const_initializer v ->
        Printf.sprintf "value assigned to %s is not constant" v
      | Failure m -> m
      | e -> raise e
    in
    failwith (Printf.sprintf "Type error in %s: %s" prefix error)

(** Type représentant l'environnement, les variables accessibles et les fonctions
    du programme *)
type environment = {
  functions: fun_def Env.t;
  variables: typ Env.t
}

(** Vérification du bon typage d'un programme.
    Renvoie l'ast typé *)
let typecheck_program (prog: prog) =
  (* Vérification du bon typage et calcul du type d'une expression dans un
     environnement donné. *)
  let type_expr env e =
    let rec type_expr e =
      match e.expr with
      | Cst _ -> { e with t = Int; const = true }
      | BCst _ -> { e with t = Bool; const = true }
      | InitList l ->
        let t = e.t in
        let cst = ref true in
        let typed_l = List.map (fun e ->
          let e' = type_expr e in
          cst := !cst && e'.const;
          if t = e'.t then
            e'
          else
            raise (Initializer_list_mismatch (t, e'.t))
        ) l
        in
        { t; const = !cst; expr = InitList typed_l }
      | UnaryOperator(op, e) ->
        let e' = type_expr e in
        let t = match op, e'.t with
          | Minus, Int -> Int
          | Not, Bool  -> Bool
          | BNot, Int  -> Int
          | _, _ -> raise (Unary_operator_mismatch (op, e'.t))
        in
        { t; const = e'.const; expr = UnaryOperator(op, e') }
      | BinaryOperator(op, e1, e2) ->
        let e1' = type_expr e1 in
        let e2' = type_expr e2 in
        let t1 = e1'.t in
        let t2 = e2'.t in
        let t = match op, t1, t2 with
          | (Add | Sub | Mult | Div | Mod | BAnd | BOr | BXor | Lsl | Asr), Int, Int -> Int
          | (Lt | Leq | Gt | Geq), Int, Int -> Bool
          | (And | Or), Bool, Bool -> Bool
          | (Eq | Neq), _, _ when t1 = t2 -> Bool
          | _, _, _ -> raise (Binary_operator_mismatch (op, t1, t2))
        in
        { t; const = e1'.const && e2'.const; expr = BinaryOperator(op, e1', e2') }
      | Get(x) ->
        let t =  match Env.find_opt x env.variables with
        | Some (Tab (t, _)) -> Ptr t
        | Some t -> t
        | None -> raise (Undefined_variable x)
        in
        { t; const = false; expr = Get x }
      | Read(ptr, offset) ->
        let ptr' = type_expr ptr in
        let offset' = type_expr offset in
        begin match ptr'.t, offset'.t with
          | Ptr t, Int -> { t; const = false; expr = Read (ptr', offset') }
          | _, _ -> failwith "Using normal type as a pointer (TODO)"
        end
      | Call(f, args) -> type_call f args
    (* Vérification du bon typage d'un appel de fonction *)
    and type_call f args =
      let func =
        match Env.find_opt f env.functions with
        | Some f -> f
        | None -> raise (Undefined_function f)
      in
      let rec typecheck_args params args =
        match params, args with
        | [], [] -> []
        | [], _ -> failwith ("too many arguments in call to " ^ func.name)
        | _, [] -> failwith ("missing arguments in call to " ^ func.name)
        | (p, tp)::params', e::args' ->
          let e' = type_expr e in
          if e'.t <> tp then
            raise (Bad_function_arg (func.name, p, tp, e'.t))
          else
            e' :: typecheck_args params' args'
      in
      let args' = typecheck_args func.params args in
      { t = func.return; const = false; expr = Call (f, args') }
    in
    type_expr e
  in

  (* Vérifie la cohérence des types d'une de déclaration de variable et ajoute
     cette variable à l'environnement. *)
  let typecheck_var_decl (env, local_vars) ((x, tv, e): var_decl) =
    if Env.mem x local_vars then
      failwith ("redefinition of variable " ^ x)
    else
      let e' = type_expr env e in
      let te = e'.t in
      match tv, te with
      | Void, _ -> raise (Void_variable x)
      | Tab _, _ ->
        let env' = { env with variables = Env.add x tv env.variables } in
        let local_env' = Env.add x () local_vars in
        (env', local_env'), (x, tv, e')
      | _, _ when tv = te -> 
        let env' = { env with variables = Env.add x tv env.variables } in
        let local_env' = Env.add x () local_vars in
        (env', local_env'), (x, tv, e')
      | _, _ -> raise (Bad_assignement (tv, x, te))        
  in

  let typecheck_function env (fdef: fun_def) =
    let returns = ref false in
    (* Vérification du bon typage des instructions d'un bloc *)
    let rec typecheck_block env b =
      let env = ref env in
      let local_vars = ref Env.empty in
      List.map (typecheck_instr env local_vars) b
    (* Vérification du bon typage d'une instruction *)
    and typecheck_instr env local_vars = function
      | Putchar(e) ->
          let e' = type_expr !env e in
          if e'.t = Int then
            Putchar e'
          else
            failwith "putchar expects an integer (ascii code) argument"
      | Decl(l) ->
          let (env', local_vars'), l' = List.fold_left_map typecheck_var_decl (!env, !local_vars) l in
          env := env';
          local_vars := local_vars';
          Decl l'
      | Set(x, e) ->
        let e' = type_expr !env e in
        let t_var = match Env.find_opt x !env.variables with
          | Some t -> t
          | None -> raise (Undefined_variable x)
        in
        if t_var = e'.t then
          Set (x, e')
        else
          raise (Bad_assignement (t_var, x, e'.t))
      | Write (ptr, offset, e) ->
        let ptr' = type_expr !env ptr in
        let offset' = type_expr !env offset in
        let e' = type_expr !env e in
        begin match ptr'.t, offset'.t, e'.t with
        | Ptr t, Int, te when t = te -> Write (ptr', offset', e')
        | _, _, _ -> failwith "TODO : bad mem assignement, typechecker.ml"
        end
      | If(cond, t, f) ->
          let cond' = type_expr !env cond in
          if cond'.t <> Bool then
            raise (Bad_condition ("an if", cond'.t))
          else
            let t' = typecheck_block !env t in
            let f' = typecheck_block !env f in
            If (cond', t', f')
      | While(cond, b) ->
        let cond' = type_expr !env cond in
        if cond'.t <> Bool then
          raise (Bad_condition ("a loop", cond'.t))
        else
          let b' = typecheck_block !env b in
          While (cond', b')
      | Return(e) ->
        let e' = type_expr !env e in
        begin match fdef.return, e'.t with
        | Void, _ -> failwith "cannot return a value from a void function"
        | t1, t2 when t1 <> t2 -> raise (Return_value_mismatch (t1, t2))
        | _, _ -> returns := true; Return (e')
        end
      | Expr(e) -> Expr (type_expr !env e)
      | Block(b) -> Block (typecheck_block !env b)
    in
    
    if Env.mem fdef.name env.functions then
      failwith ("redefinition of function " ^ fdef.name)
    else begin
      let param_env =
        List.fold_left (fun env (x, ty) -> 
          if ty = Void then
            raise (Void_variable x)
          else
            Env.add x ty env
        ) env.variables fdef.params
      in
      let env' = { functions = Env.add fdef.name fdef env.functions; variables = param_env } in
      let body' = typecheck_block env' fdef.body in
      if fdef.return <> Void && not !returns then
        failwith "a non void function should return a value"
      else
        env', { fdef with body = body' }
    end
  in

  (* On vérifie les déclarations de variables globales et le type des fonction *)
  let _, ast = List.fold_left_map (fun (env, vars) decl ->
    match decl with
    | Variable d -> begin
      try
        let env', (v, t, e) = typecheck_var_decl (env, vars) d in
        if not e.const then
          raise (Not_const_initializer v)
        else
          env', Variable (v, t, e)
      with
        e -> error Global e
      end
    | Function f -> begin
      try
        let env', f' = typecheck_function env f in
        (env', vars), Function f'
      with
        e -> error (Local f.name) e
      end
  ) ({ functions=Env.empty; variables=Env.empty }, Env.empty) prog
  in
  ast

