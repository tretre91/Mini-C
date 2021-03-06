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

(** Exception levée lorsqu'une conversion entre 2 types n'est pas valide *)
exception Bad_cast of typ * typ

(** Exception levée lorsqu'une variable qui n'est pas un pointeur est utilisée comme un pointeur *)
exception Not_a_pointer of typ

(** Type indiquant où l'on se trouve dans un programme *)
type scope =
  | Global
  | Local of string

(** Lève une exception qui contient un message d'erreur indiquant le nom de la
    fonction où l'erreur a eu lieu. *)
let error scope exn =
  let string_of_integral_type = function
    | Char  -> "char"
    | Short -> "short"
    | Int   -> "int"
    | Long  -> "long"
  in
  let rec string_of_typ = function
    | Mut t -> string_of_plain_type t
    | Const t -> "const " ^ (string_of_plain_type t) 
  and string_of_plain_type = function
    | Integer t -> string_of_integral_type t
    | Float -> "float"
    | Double -> "double"
    | Bool -> "bool"
    | Void -> "void"
    | Ptr t -> string_of_typ t ^ " ptr"
    | Tab (t, _) -> Printf.sprintf "%s[]" (string_of_typ t)
  in
  let prefix =
    match scope with
    | Global -> "global scope"
    | Local f -> "function " ^ f
  in
  let open Printf in
  let error =
    match exn with
    | Bad_assignement (tv, var, te) ->
      sprintf "cannot assign a value of type %s to variable '%s' of type %s" (string_of_typ te) var (string_of_typ tv)
    | Bad_condition (context, te) ->
      sprintf "expecting a bool expression in %s's condition, got %s" context (string_of_typ te)
    | Undefined_variable var ->
      "undefined variable " ^ var
    | Void_variable var ->
      sprintf "'void %s': cannot define a variable of type void" var
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
      sprintf "%s operator expects its arguments to be %s, got %s and %s" op_type expected st1 st2
    | Bad_function_arg (f, param, tp, te) ->
      let stp = string_of_typ tp in
      let ste = string_of_typ te in
      sprintf "in call to %s, parameter '%s' expects an argument of type %s, got %s" f param stp ste
    | Return_value_mismatch (expected, got) ->
      sprintf "cannot return a value of type %s from a %s function" (string_of_typ expected) (string_of_typ got)
    | Initializer_list_mismatch (expr_t, list_t) ->
      sprintf "cannot use a value of type %s in an initializer list of type %s" (string_of_typ expr_t) (string_of_typ list_t)
    | Not_const_initializer v ->
      sprintf "value assigned to %s is not constant" v
    | Bad_cast (t1, t2) ->
      sprintf "cannot convert a value of type %s to type %s" (string_of_typ t1) (string_of_typ t2)
    | Not_a_pointer t ->
      sprintf "variable of type %s cannot be accessed as a pointer" (string_of_typ t)
    | Failure m -> m
    | e -> raise e
  in
  failwith (sprintf "Type error in %s: %s" prefix error)

(** Type représentant l'environnement, les variables accessibles et les fonctions
    du programme *)
type environment = {
  functions: fun_def Env.t;
  variables: typ Env.t
}

let get_type = function
  | Mut t -> t
  | Const t -> t

(** Trouve la conversion appropriée vers un type cible pour une expression donnée *)
let get_cast e t =
  let source_type = get_type e.t in
  let target_type = get_type t in
  match source_type, target_type with
  | _, _ when source_type = target_type -> e
  | Integer _, (Integer _ | Float | Double | Bool | Ptr _)
  | Float, (Integer _ | Double | Bool)
  | Double, (Integer _ | Float | Bool)
  | Bool, (Integer _ | Float)
  | Ptr _, (Integer _ | Ptr _) -> { t; const = e.const; expr = Cast (e, e.t, t) }
  | _, _ -> raise (Bad_cast (e.t, t))

(** Convertit une expression en une expression entière *)
let get_integer e =
  match get_type e.t with
  | Integer _ -> e
  | Bool -> get_cast e (Mut (Integer Int))
  | _ -> failwith "expression is not an integer"

let is_integral = function
  | Mut Integer _ -> true
  | Const Integer _ -> true
  | _ -> false

(** Trouve le type résultat d'une opération binaire *)
let unify t1 t2 =
  let unify_integral_type t1 t2 =
    match t1, t2 with
    | Long, _
    | _, Long -> Long
    | _, _ -> Int
  in
  let t1 = get_type t1 in
  let t2 = get_type t2 in
  let t = match t1, t2 with
    | Integer t1', Integer t2' -> Integer (unify_integral_type t1' t2')
    | _, _ when t1 = t2 -> t1
    | Bool, Integer t
    | Integer t, Bool -> Integer (unify_integral_type t Int)
    | Bool, _ -> t2
    | _, Bool -> t1
    | Ptr _, (Double | Float)
    | (Double | Float), Ptr _ -> failwith "types cannot be unified" (* TODO *)
    | Ptr _, _ -> t1
    | _, Ptr _ -> t2
    | Double, _
    | _, Double -> Double
    | Float, _
    | _, Float -> Float
    | _, _ -> failwith "types cannot be unified"
  in
  Const t

(** Vérification du bon typage d'un programme.
    Renvoie l'ast typé *)
let typecheck_program (prog: prog) =
  let type_constant c =
    let t = match c with
      | CInteger (t, _) -> Integer t
      | CFloat _ -> Float
      | CDouble _ -> Double
      | CBool _ -> Bool
      | CString _ -> Ptr (Const (Integer Char))
      | CIList _ -> failwith "never reached, initializer lists are not treated as constants in this step"
    in
    Const t
  in
  (* Vérification du bon typage et calcul du type d'une expression dans un
     environnement donné. *)
  let type_expr env e =
    let rec type_expr e =
      match e.expr with
      | Cst c -> { e with t = type_constant c; const = true }
      | Cast (expr, _, to_) ->
        let e' = type_expr expr in
        { e with const = e'.const; expr = Cast (e', e'.t, to_) }
      | InitList l ->
        let t = e.t in
        let cst = ref true in
        let typed_l = List.map (fun e ->
          let e' = type_expr e in
          cst := !cst && e'.const;
          try get_cast e' t
          with Bad_cast _ -> raise (Initializer_list_mismatch (t, e'.t))
        ) l
        in
        { t; const = !cst; expr = InitList typed_l }
      | UnaryOperator(op, e) ->
        let e = type_expr e in
        begin
          try
            let e' = match op with
              | Minus ->
                let t = unify e.t (Const (Integer Int)) in (* TODO : pourquoi ? *)
                get_cast e t
              | Not  -> get_cast e (Const Bool)
              | BNot -> get_integer e
            in
            { t = e'.t; const = e'.const; expr = UnaryOperator(op, e') }
          with
            Failure _ | Bad_cast _ -> raise (Unary_operator_mismatch (op, e.t))
        end
      | BinaryOperator(op, e1, e2) ->
        let e1 = type_expr e1 in
        let e2 = type_expr e2 in
        let e1', e2', t = try
          match op with
          | Add | Sub | Mult | Div | Mod | BAnd | BOr | BXor ->
            let t = unify e1.t e2.t in
            let e1' = get_cast e1 t in
            let e2' = get_cast e2 t in
            e1', e2', t
          | Lsl | Asr when is_integral e1.t && is_integral e2.t ->
            e1, e2, e1.t
          | Lt | Leq | Gt | Geq | Eq | Neq ->
            let t = unify e1.t e2.t in
            let e1' = get_cast e1 t in
            let e2' = get_cast e2 t in
            e1', e2', (Const Bool)
          | And | Or ->
            let target_type = Const Bool in
            let e1' = get_cast e1 target_type in
            let e2' = get_cast e2 target_type in
            e1', e2', target_type
          | _ -> failwith ""
        with
          | _ -> raise (Binary_operator_mismatch (op, e1.t, e2.t))
        in
        { t; const = e1'.const && e2'.const; expr = BinaryOperator(op, e1', e2') }
      | Get(x) ->
        let t =  match Env.find_opt x env.variables with
        | Some (Mut Tab (t, _) | Const Tab (t, _)) -> (Const (Ptr t))
        | Some t -> t
        | None -> raise (Undefined_variable x)
        in
        { t; const = false; expr = Get x }
      | Read(ptr, offset) ->
        let ptr' = type_expr ptr in
        let offset' = get_cast (type_expr offset) (Const (Integer Int)) in
        begin match get_type ptr'.t with
          | Ptr t ->
            let offset' = get_integer offset' in
            { t; const = false; expr = Read (ptr', offset') }
          | _ -> raise (Not_a_pointer ptr'.t)
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
          try
            get_cast e' tp :: typecheck_args params' args'
          with
            Bad_cast _ -> raise (Bad_function_arg (func.name, p, tp, e'.t))
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
      | (Const Void | Mut Void), _ -> raise (Void_variable x)
      | Const (Tab (t, n)),  _ ->
        begin match get_type t, e'.expr with
          | Integer Char, Cst (CString addr) ->
            (* cas char tab[] = "blabla" *)
            let s = Minic.get_static_string addr in
            let length = Cst (CInteger (Int, Int64.of_int (String.length s))) in
            let t' = Const (Tab (t, { t = Const (Integer Int); const = true; expr = length })) in
            let env' = { env with variables = Env.add x t' env.variables } in
            let local_env' = Env.add x () local_vars in
            (env', local_env'), (x, t', e')
          | _, InitList _ ->
            let n' = get_integer (type_expr env n) in
            let t' = Const (Tab (t, n')) in
            let env' = { env with variables = Env.add x t' env.variables } in
            let local_env' = Env.add x () local_vars in
            (env', local_env'), (x, t', e')
          | _, _ -> raise (Bad_assignement (tv, x, te))
        end
      | _, _ ->
        try
          let e' = get_cast e' tv in
          let env' = { env with variables = Env.add x tv env.variables } in
          let local_env' = Env.add x () local_vars in
          (env', local_env'), (x, tv, e')
        with
          Bad_cast _ -> raise (Bad_assignement (tv, x, te))        
  in

  let typecheck_function env (fdef: fun_def) =
    (* Si la fonction est un prédéclaration on considère qu'elle renvoie une valeur *)
    let returns = ref (fdef.is_forward_decl) in
    (* Vérification du bon typage des instructions d'un bloc *)
    let rec typecheck_block env b =
      let env = ref env in
      let local_vars = ref Env.empty in
      List.map (typecheck_instr env local_vars) b
    (* Vérification du bon typage d'une instruction *)
    and typecheck_instr env local_vars = function
      | Decl(l) ->
        let (env', local_vars'), l' = List.fold_left_map typecheck_var_decl (!env, !local_vars) l in
        env := env';
        local_vars := local_vars';
        Decl l'
      | Set(x, e) ->
        let e' = type_expr !env e in
        let t_var = match Env.find_opt x !env.variables with
          | Some (Const _) -> failwith (Printf.sprintf "Cannot modify const variable '%s'" x)
          | Some (Mut t) -> Mut t
          | None -> raise (Undefined_variable x)
        in
        begin
          try Set (x, get_cast e' t_var)
          with Bad_cast _ -> raise (Bad_assignement (t_var, x, e'.t))
        end
      | Write (ptr, offset, e) ->
        let ptr' = type_expr !env ptr in
        let offset' = get_cast (type_expr !env offset) (Const (Integer Int)) in
        let e' = type_expr !env e in
        begin try
          let offset' = get_integer offset' in
          match get_type ptr'.t with
          | Ptr (Mut t) ->
            let e' = get_cast e' (Mut t) in
            Write (ptr', offset', e')
          | Ptr (Const _) -> failwith "Cannot write to a pointer to const value"
          | _ -> raise (Not_a_pointer ptr'.t)
        with
          Bad_cast (from, to_) -> raise (Bad_assignement (to_, "[ptr]", from))
        end
      | If(cond, t, f) ->
          let cond' = type_expr !env cond in
          begin
            try
              let cond'' = get_cast cond' (Const Bool) in
              let t' = typecheck_block !env t in
              let f' = typecheck_block !env f in
              If (cond'', t', f')
            with
              Bad_cast _ -> raise (Bad_condition ("an if", cond'.t))
          end
      | While(cond, b) ->
        let cond' = type_expr !env cond in
        begin
          try
            let cond'' = get_cast cond' (Const Bool) in
            let b' = typecheck_block !env b in
            While (cond'', b')
          with
            Bad_cast _ -> raise (Bad_condition ("a while loop", cond'.t))
        end
      | Return(e) ->
        let e' = type_expr !env e in
        begin match fdef.return, e'.t with
        | (Mut Void | Const Void), _ -> failwith "cannot return a value from a void function"
        | t1, t2 ->
          try
            let e'' = get_cast e' fdef.return in
            returns := true;
            Return (e'')
          with
            Bad_cast _ -> raise (Return_value_mismatch (t1, t2))
        end
      | Expr(e) -> Expr (type_expr !env e)
      | Block(b) -> Block (typecheck_block !env b)
    in
    
    if Env.mem fdef.name env.functions then
      failwith ("redefinition of function " ^ fdef.name)
    else begin
      let param_env =
        List.fold_left (fun env (x, ty) -> 
          if get_type ty = Void then
            raise (Void_variable x)
          else
            Env.add x ty env
        ) env.variables fdef.params
      in
      let env' = { env with functions = Env.add fdef.name fdef env.functions } in
      let body' = typecheck_block { env' with variables = param_env } fdef.body in
      if get_type fdef.return <> Void && not !returns then
        failwith "a non void function should return a value"
      else
        env', { fdef with body = body' }
    end
  in

  (* Table de hachage contenant les fonctions définies dans le programme *)
  let functions = Hashtbl.create 16 in

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
    | Function f when f.is_forward_decl -> begin
      match Hashtbl.find_opt functions f.name with
      | None ->
        begin try
          let env', f' = typecheck_function env f in
          Hashtbl.add functions f.name f';
          (env', vars), Function f'
        with
          e -> error (Local f.name) e
        end
      | Some f when f.is_forward_decl -> error Global (Failure (Printf.sprintf "trying to declare function %s which is already declared" f.name))
      | Some f when not f.is_forward_decl -> error Global (Failure (Printf.sprintf "redeclaration of function %s" f.name))
      | _ -> failwith __LOC__
      end
    | Function f -> begin
      let add_func f =
        try
          let env' = { env with functions = Env.remove f.name env.functions } in
          let env'', f' = typecheck_function env' f in
          (env'', vars), Function f'
        with
          e -> error (Local f.name) e
      in
      match Hashtbl.find_opt functions f.name with
      | None -> add_func f
      | Some f when f.is_forward_decl -> add_func f
      | Some _ -> error Global (Failure (Printf.sprintf "redefinition of function %s" f.name))
      end
  ) ({ functions=Env.empty; variables=Env.empty }, Env.empty) prog
  in
  
  let has_attribute f attribute =
    List.mem attribute f.attributes
  in

  (* Vérification des prédéclarations (TODO) *)
  Hashtbl.iter (fun _ f ->
    let is_extern = has_attribute f "extern" in
    if f.is_forward_decl then begin
      if not is_extern then
        failwith ("No definition found for function " ^ f.name)
      end
    else
      if is_extern then
        failwith (Printf.sprintf "Cannot define a function which is declared extern (%s)" f.name)
  ) functions;
  
  ast
