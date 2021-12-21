{

  open Lexing
  open Minic_parser

  (* Fonction auxiliaire pour rassembler les mots-clés 
     À COMPLÉTER
   *)
  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "return",   RETURN;
        "true",     BOOL_CST true;
        "false",    BOOL_CST false;
        "int",      INT;
        "bool",     BOOL;
        "void",     VOID;
        "if",       IF;
        "else",     ELSE;
        "while",    WHILE;
        "putchar",  PUTCHAR;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
  
  (* Quitte le programme et affiche un message d'erreur indiquant l'emplacement de l'erreur *)
  let error message pos =
    Printf.fprintf stderr "error at (%d, %d): %s" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) message;
    exit 1
}

(* Règles auxiliaires *)
let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*
let comment = "//"[^'\n']*

(* Règles de reconnaissance 
   À COMPLÉTER
*)
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | comment
      { token lexbuf }
  | "/*"
      { multiline_comment (lexeme_start_p lexbuf) lexbuf }
  | number as n
      { CST(int_of_string n) }
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
  | "+"
      { ADD }
  | "-"
      { SUB }
  | "*"
      { MUL }
  | "/"
      { DIV }
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
