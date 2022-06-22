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
        "int",      INT;
        "long",     LONG;
        "float",    FLOAT;
        "double",   DOUBLE;
        "bool",     BOOL;
        "void",     VOID;
        "if",       IF;
        "else",     ELSE;
        "while",    WHILE;
        "for",      FOR;
        "extern",   EXTERN;
        "const",    CONST;
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
        | 'a' -> 7
        | 'b' -> 8
        | 'f' -> 12
        | 'n' -> 10
        | 'r' -> 13
        | 't' -> 9
        | 'v' -> 11
        | ('\'' | '\"' | '?' | '\\') as c -> Char.code c
        | _ -> failwith ("invalid escape sequence " ^ s)
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

  let strings = Hashtbl.create 16
  let strings_offset = ref 8 (* On commence à écrire nos données à l'adresse 8 (au lieu de 0 qui est réservée *)

  (** fonction utilisée dans pour réinitialiser les données entre 2 tests *)
  let reset_strings () =
    Hashtbl.clear strings;
    strings_offset := 8

  let add_string s =
    match Hashtbl.find_opt strings s with
    | Some addr -> addr
    | None -> begin
        let curr_offset = !strings_offset in
        strings_offset := curr_offset + String.length s;
        Hashtbl.add strings s curr_offset;
        curr_offset
      end

  (* Quitte le programme et affiche un message d'erreur indiquant l'emplacement de l'erreur *)
  let error message pos =
    Printf.fprintf stderr "error at (%d, %d): %s\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) message;
    exit 1
}

(* Règles auxiliaires *)
let digit = ['0'-'9']
let octal = ['0'-'7']
let hex = ['a'-'f''A'-'F''0'-'9']

let hex_char = "\\x" (hex | hex hex)
let oct_char = "\\" (octal | octal octal | octal octal octal)
let chr = (_ | hex_char | oct_char | "\\"['a'-'z'])

let number = digit+
let integer = '-'? (number | "0x" hex+ | "0b" ['0''1']+)

let float = '-'? (number '.' (number ?) | '.' number)

let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | '_' | digit)*
let space = [' ' '\t' '\r']

(* Règles de reconnaissance *)
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | space+
      { token lexbuf }
  | "'" (chr as c) "'"
      { CST (CInteger (Char, Int64.of_int (int_of_char (char_of_string c)))) }
  | "\"" {
        let str = str (Buffer.create 16) (lexeme_start_p lexbuf) lexbuf in
        let address = add_string str in
        CST (CString address)
    }
  | integer as n
      { let cst = get_int_constant n in CST cst }
  | (float as f) 'f'
      { CST (CFloat (float_of_string f)) }
  | float as d
      { CST (CDouble (float_of_string d)) }
  | ident as id
      { keyword_or_ident id }
  | (ident "::" ident) as attr
      { SCOPED_ATTR (attr) }
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
and str buf start_pos = parse
  | "\""
      { Buffer.add_char buf '\000'; Buffer.contents buf }
  | chr as c
      { Buffer.add_char buf (char_of_string c); str buf start_pos lexbuf }
  | eof
      { error "unclosed string litteral" start_pos }
