{

  open Lexing
  open Minic_parser

  (* Fonction auxiliaire pour rassembler les mots-clés *)
  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "return",   RETURN;
        "true",     CST (CBool true);
        "false",    CST (CBool false);
        "char",     CHAR;
        "short",    SHORT;
        "long",     LONG;
        "int",      INT;
        "bool",     BOOL;
        "void",     VOID;
        "if",       IF;
        "else",     ELSE;
        "while",    WHILE;
        "for",      FOR;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
  
  let char_of_string s =
    let len = String.length s in
    match s.[0] with
    | '\\' ->
      let code = match s.[1] with
        | 'x' -> int_of_string ("0x" ^ (String.sub s 2 (len - 2)))
        | '0'..'7' -> int_of_string ("0o" ^ (String.sub s 1 (len - 1)))
        | 'n' -> 10
        | _ -> failwith "TODO : special escape characters"
      in
      Char.chr code
    | _ as c -> c

  let get_int_constant =
    let max_32_bit = Int64.of_int32 (Int32.max_int) in
    let min_32_bit = Int64.of_int32 (Int32.min_int) in
    fun n ->
      let value = Int64.of_string n in
      if value > max_32_bit || value < min_32_bit then
        Minic_ast.CInteger (Long, value)
      else
        Minic_ast.CInteger (Int, value)

  (* Quitte le programme et affiche un message d'erreur indiquant l'emplacement de l'erreur *)
  let error message pos =
    Printf.fprintf stderr "error at (%d, %d): %s" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) message;
    exit 1
}

(* Règles auxiliaires *)
let digit = ['0'-'9']
let octal = ['0'-'7']
let hex = ['a'-'f''A'-'F''0'-'9']

let valid_escapes = "\\" ['n'] (* TODO *)
let hex_char = "\\x" (hex | hex hex)
let oct_char = "\\" (octal | octal octal | octal octal octal)
let chr = (_ | hex_char | oct_char | valid_escapes)
let char = "'" chr "'"

let number = '-'? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*
let comment = "//"[^'\n']*

(* Règles de reconnaissance *)
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | comment
      { token lexbuf }
  | "/*"
      { multiline_comment (lexeme_start_p lexbuf) lexbuf }
  | "'" (chr as c) "'"
      { CST (CInteger (Char, Int64.of_int (int_of_char (char_of_string c)))) }
  | number as n
      { let cst = get_int_constant n in CST cst }
  | ident as id
      { keyword_or_ident id }
  | ";"
      { SEMI }
  | ","
      { COMMA }
  | "="
      { SET }
  | "("
      { LPAR }
  | ")"
      { RPAR }
  | "{"
      { BEGIN }
  | "}"
      { END }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | "+"
      { ADD }
  | "-"
      { SUB }
  | "*"
      { MUL }
  | "/"
      { DIV }
  | "%"
      { MOD }
  | "<<"
      { LSL }
  | ">>"
      { ASR }
  | "=="
      { EQ }
  | "!="
      { NEQ }
  | "<"
      { LT }
  | "<="
      { LEQ }
  | ">"
      { GT }
  | ">="
      { GEQ }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "!"
      { NOT }
  | "&"
      { BAND }
  | "|"
      { BOR }
  | "^"
      { BXOR }
  | "~"
      { BNOT }
  | _
      { error ("Unknown character : " ^ (lexeme lexbuf)) (lexeme_start_p lexbuf) }
  | eof
      { EOF }
and multiline_comment start_pos = parse
  | ['\n']
      { new_line lexbuf; multiline_comment start_pos lexbuf }
  | "*/"
      { token lexbuf }
  | _
      { multiline_comment start_pos lexbuf }
  | eof
      { error "Forgot to close this multiline comment" start_pos }
