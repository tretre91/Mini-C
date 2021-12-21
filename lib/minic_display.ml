open Minic_ast
open Printf

let print_program (prog: prog) (out: out_channel) =
  let string_of_typ = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
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

  let rec string_of_expr (e: expr) =
    match e with
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
    | Call (f, args) -> sprintf "%s(%s)" f (String.concat ", " (List.map string_of_expr args))
  in

  let rec print_instr (indent: string) (i: instr) =
    output_string out indent; 
    match i with
    | Putchar e -> fprintf out "putchar(%s);" (string_of_expr e)
    | Set(x, e) -> fprintf out "%s = %s;" x (string_of_expr e)
    | If(e, s1, s2) ->
      fprintf out "if (%s) {\n" (string_of_expr e);
      print_seq (indent ^ "  ") s1;
      fprintf out "%s} else {\n" indent;
      print_seq (indent ^ "  ") s2;
      fprintf out "%s}" indent
    | While(e, s) ->
      fprintf out "while (%s) {\n" (string_of_expr e);
      print_seq (indent ^ "  ") s;
      fprintf out "%s}" indent
    | Return e -> fprintf out "return %s;" (string_of_expr e)
    | Expr e -> fprintf out "%s;" (string_of_expr e)
  and print_seq (indent: string) (s: seq) =
    List.iter (fun i -> print_instr indent i; output_char out '\n') s
  in

  let print_declaration_list (indent: string) (decl: (string * typ * expr) list) =
    List.iter (fun (x, t, e) -> fprintf out "%s%s %s = %s;\n" indent (string_of_typ t) x (string_of_expr e)) decl
  in

  let print_fun_def (fdef: fun_def) =
    let params =
      String.concat ", " (List.map (fun (x, t) -> sprintf "%s %s" (string_of_typ t) x) fdef.params)
    in
    fprintf out "%s %s(%s) {\n" (string_of_typ fdef.return) fdef.name params;
    print_declaration_list "  " fdef.locals;
    print_seq "  " fdef.code;
    fprintf out "}\n\n";
  in

  print_declaration_list "" prog.globals;
  if prog.globals <> [] then
    fprintf out "\n";
  List.iter print_fun_def prog.functions
