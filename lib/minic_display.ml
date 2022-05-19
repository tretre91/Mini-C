open Minic_ast
open Printf

let print_program (prog: prog) (out: out_channel) =
  let rec string_of_typ = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | Ptr t -> string_of_typ t ^ "*"
  in

  let string_of_unop = function
    | Minus -> "-"
    | Not   -> "!"
    | BNot  -> "~"
  in
  
  let string_of_binop = function
    | Add  -> "+"
    | Sub  -> "-"
    | Mult -> "*"
    | Div  -> "/"
    | Mod  -> "%"
    | Eq   -> "=="
    | Neq  -> "!="
    | Lt   -> "<"
    | Leq  -> "<="
    | Gt   -> ">"
    | Geq  -> ">="
    | And  -> "&&"
    | Or   -> "||"
    | BAnd -> "&"
    | BOr  -> "|"
    | BXor -> "^"
    | Lsl  -> "<<"
    | Asr  -> ">>"
  in

  let rec string_of_expr e =
    match e.expr with
    | Cst n -> string_of_int n
    | BCst b -> string_of_bool b
    | UnaryOperator(op, e) ->
      let s = string_of_expr e in
      string_of_unop op ^ s
    | BinaryOperator(op, e1, e2) ->
      let s1 = string_of_expr e1 in
      let s2 = string_of_expr e2 in
      let s_op = string_of_binop op in
      sprintf "(%s %s %s)" s1 s_op s2
    | Get x -> x
    | Read e -> sprintf "*(%s)" (string_of_expr e)
    | Call (f, args) -> sprintf "%s(%s)" f (String.concat ", " (List.map string_of_expr args))
  in


  let print_assignement x e =
    fprintf out "%s = %s" x (string_of_expr e)
  in

  (* Affiche une déclaration de variables *)
  let print_var_decl d =
    let rec aux = function
      | [] -> ()
      | [(x, _, e)] -> 
        print_assignement x e;
        output_char out ';'
      | (x, _, e)::tl ->
        print_assignement x e;
        fprintf out ", ";
        aux tl
    in
    let _, t, _ = List.hd d in
    fprintf out "%s " (string_of_typ t);
    aux d
  in
  

  (* Affiche une instruction avec le bon niveau d'indentation *)
  let rec print_instr (spaces: int) (i: instr) =
    match i with
    | Putchar e ->
      fprintf out "putchar(%s);" (string_of_expr e)
    | Decl d ->
      print_var_decl d
    | Set(x, e) ->
      print_assignement x e;
      output_char out ';'
    | Write (p, e) ->
      fprintf out "%s = %s;" (string_of_expr p) (string_of_expr e)
    | If(e, t, f) ->
      fprintf out "if (%s) " (string_of_expr e);
      print_block (spaces + 2) t;
      fprintf out " else ";
      print_block (spaces + 2) f
    | While(e, b) ->
      fprintf out "while (%s) " (string_of_expr e);
      print_block (spaces + 2) b
    | Return e ->
      fprintf out "return %s;" (string_of_expr e)
    | Expr e ->
      fprintf out "%s;" (string_of_expr e)
    | Block b ->
      print_block (spaces + 2) b
  (* Affiche un bloc de code *)
  and print_block (spaces: int) (b: block) =
    let indent = String.make spaces ' ' in
    fprintf out "{\n";
    List.iter (fun i -> output_string out indent; print_instr spaces i; output_char out '\n') b;
    fprintf out "%s}" (String.make (spaces - 2) ' ')
  in

  (* Affiche une fonction *)
  let print_fun_def (fdef: fun_def) =
    let params =
      String.concat ", " (List.map (fun (x, t) -> sprintf "%s %s" (string_of_typ t) x) fdef.params)
    in
    fprintf out "%s %s(%s) " (string_of_typ fdef.return) fdef.name params;
    print_block 2 fdef.body;
    output_char out '\n'
  in

  (* On affiche les déclarations de variables globales et de fonctions *)
  List.iter (fun d ->
    begin match d with
    | Variable v -> print_var_decl [v]
    | Function f -> print_fun_def f
    end;
    output_string out "\n\n"
  ) prog
