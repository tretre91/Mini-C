{

  open Compile_args

  exception Unclosed_string_literal
  exception Unclosed_if_directive
  
  (** Renvoie le contenu d'un input_channel sous forme de chaîne de caractères *)
  let get_channel_contents ic =
    let chunk_size = 128 in
    let buf = Buffer.create chunk_size in
    let rec get_channel_contents_aux () =
      try
        Buffer.add_channel buf ic chunk_size;
        get_channel_contents_aux ()
      with
        End_of_file -> Buffer.contents buf
    in
    get_channel_contents_aux ()

  (** Indique si un dossier existe *)
  let dir_exists dir =
    Sys.file_exists dir && Sys.is_directory dir

  (** Ouvre une pipe et renvoie les deux côtés sous forme de channel *)
  let open_pipe () =
    let ifd, ofd = Unix.pipe () in
    let ic = Unix.in_channel_of_descr ifd in
    let oc = Unix.out_channel_of_descr ofd in
    ic, oc

  (** Recherche un fichier parmi une liste de répértoires *)
  let rec search_include directories file =
    match directories with
    | [] -> None
    | dir::tl ->
      let path = Filename.concat dir file in
      if dir_exists dir && Sys.file_exists path then
        Some path
      else
        search_include tl file

  let system_include_folder =
    let root_dir =
       Sys.executable_name
    |> Filename.dirname (* bin *)
    |> Filename.dirname (* minic *)
    in
    Filename.concat root_dir "include"

  let user_include_paths = ref []

  (** Table de hachage contenant les macros définies *)
  let defines = Hashtbl.create 16

  (** Indique si une macro est définie *)
  let defined = Hashtbl.mem defines

  let init args =
    List.iter (fun (macro, value) ->
      Hashtbl.add defines macro (Option.value value ~default:"")
    ) args.definitions;
    user_include_paths := Sys.getcwd () :: args.include_paths

  let if_depth = ref 0

  (** Creates a lexbuf from a Buffer.t, the buffer is unchanged *)
  let lexbuf_of_buffer buffer =
    let index = ref 0 in
    let max_len = Buffer.length buffer in
    let get_chars b n =
      let len = min n (max_len - !index) in
      Buffer.blit buffer !index b 0 len;
      index := !index + len;
      len
    in
    Lexing.from_function get_chars

  (** Searches a file among the directories specified in the primary search paths,
      and in the secondary search paths if the first search was unsuccessful *)
  let find_file file system_path_first =
    let primary, secondary =
      if system_path_first
      then [system_include_folder], !user_include_paths
      else !user_include_paths, [system_include_folder]
    in
    match search_include primary file with
    | Some path -> path
    | None -> match search_include secondary file with
      | Some path -> path
      | None -> failwith (Printf.sprintf "No file named %s found in the include directories" file)

  (** Quitte le programme et affiche un message d'erreur indiquant l'emplacement de l'erreur *)
  let error message pos =
    let open Lexing in
    Printf.fprintf stderr "error at (%d, %d): %s\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) message;
    exit 1

  (** convertit la condition d'une directive #ifdef en bool *)
  let get_condition str =
    match int_of_string_opt str with
    | Some v -> v <> 0
    | None -> Hashtbl.mem defines str

}

let space = [' ''\t''\r']
let alpha = ['a'-'z''A'-'Z']
let alnum = ['a'-'z''A'-'Z''0'-'9''_']
let word = alnum+
let id = (alpha | '_')alnum*
let integer = ['0'-'9']+
let string = [^'#''\n']*
let condition = id | integer

let line_comment = "//"[^'\n']*

rule preprocess oc = parse
  | '\"' {
      let b = Buffer.create 64 in
      get_string b lexbuf;
      Printf.fprintf oc "\"%s\"" (Buffer.contents b);
      preprocess oc lexbuf 
    }
  | word as w {
      let s = match Hashtbl.find_opt defines w with
      | None -> w
      | Some s -> s
      in
      output_string oc s;
      preprocess oc lexbuf
    }
  | space* "#define" space+ (id as id) (space+ (string as replacement) | (space*)) '\n' {
      let pipe_ic, pipe_oc = open_pipe () in
      let buf = Lexing.from_string (Option.value replacement ~default:"") in
      let last_depth = !if_depth in
      if_depth := 0;
      preprocess pipe_oc buf;
      if_depth := last_depth;
      close_out pipe_oc;
      let replacement' = get_channel_contents pipe_ic in
      Hashtbl.replace defines id (replacement');
      close_in pipe_ic;
      preprocess oc lexbuf
    }
  | space* "#undef" space+ (id as id) space* '\n' {
      Hashtbl.remove defines id;
      preprocess oc lexbuf
    }
  | space* "#include" space+ (('"' as c) (string as file) '"' | ('<' as c) (string as file) '>') space* '\n' {
      let filename = find_file file (c = '<') in
      let ic = open_in filename in
      let buf = Lexing.from_channel ic in
      let () =
        try start_preprocessing oc buf
        with Unclosed_if_directive -> ()
      in
      close_in ic;
      preprocess oc lexbuf
    }
  | space* "#ifdef" space+ (id as macro) space* '\n' {
      if not (defined macro) then
        ignore_if 1 lexbuf
      else
        incr if_depth;
      preprocess oc lexbuf
    }
  | space* "#ifndef" space+ (id as macro) space* '\n' {
      if defined macro then
        ignore_if 1 lexbuf
      else
        incr if_depth;
      preprocess oc lexbuf
    }
  | space* "#else" space* '\n' {
      if !if_depth > 0 then
        let () = decr if_depth in
        ignore_if 1 lexbuf
      else
        error "#else without an #if directive" (Lexing.lexeme_start_p lexbuf);
      preprocess oc lexbuf
    }
  | space* "#endif" space* '\n' {
      match !if_depth with
      | 0 -> error "No matching if directive found" (Lexing.lexeme_start_p lexbuf)
      | _ -> decr if_depth;
      preprocess oc lexbuf
    }
  | eof { 
      if !if_depth > 0 then
        raise Unclosed_if_directive
    }
  | _ as c {
      output_char oc c;
      preprocess oc lexbuf
    }
(* Rule used to get a full line of input, it will merge continued lines *)
and splice_lines buf = parse
  | "\\" space* "\n" {
      Lexing.new_line lexbuf;
      splice_lines buf lexbuf
    }
  | '\"' {
      Buffer.add_char buf '\"';
      get_string buf lexbuf;
      Buffer.add_char buf '\"';
      splice_lines buf lexbuf
    }
  | eof {
      ()
    }
  | '\n' {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      splice_lines buf lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      splice_lines buf lexbuf
    }
(* Gets the contents of a string literal (TODO: check valid escape sequences) *)
and get_string buf = parse
  | "\\\n" {
      Lexing.new_line lexbuf;
      get_string buf lexbuf
    }
  | "\\\"" as s {
      Buffer.add_string buf s;
      get_string buf lexbuf
    }
  | '\"' {
      ()
    }
  | eof {
      raise Unclosed_string_literal
    }
  | _ as c {
      Buffer.add_char buf c;
      get_string buf lexbuf
    }
(* Rule used to remove comments in a line of text *)
and remove_comments buf = parse
  | '\"' {
      Buffer.add_char buf '\"';
      get_string buf lexbuf;
      Buffer.add_char buf '\"';
      remove_comments buf lexbuf
    }
  | line_comment {
      let () = Buffer.add_char buf ' ' in
      remove_comments buf lexbuf
    }
  | "/*" {
      ignore_block_comment (Lexing.lexeme_start_p lexbuf) lexbuf;
      Buffer.add_char buf ' ';
      remove_comments buf lexbuf
    }
  | '\n' {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      remove_comments buf lexbuf
    }
  | eof {
      ()
    }
  | _ as c {
      Buffer.add_char buf c;
      remove_comments buf lexbuf
    }
(* règle d'analyse des commentaires en blocs (/* ... */) *)
and ignore_block_comment start_pos = parse
  | '\n' {
      Lexing.new_line lexbuf;
      ignore_block_comment start_pos lexbuf
    }
  | "*/" { 
      ()
    }
  | _ {
      ignore_block_comment start_pos lexbuf
    }
  | eof {
      error "Forgot to close this block comment" start_pos
    }
(* Rule used to ignore a conditional block *)
and ignore_if depth = parse
  | space* ("#ifdef" | "#ifndef") space+ id space* '\n' {
      ignore_if (depth + 1) lexbuf
    }
  | space* "#endif" space* '\n' {
      if depth > 1 then
        ignore_if (depth - 1) lexbuf
    }
  | space* "#else" space* '\n' {
      if depth = 1 then
        incr if_depth
      else
        ignore_if depth lexbuf
    }
  | eof {
      error "Unclosed ifdef block" (Lexing.dummy_pos) (* TODO *)
    }
  | _ {
      ignore_if depth lexbuf
    }
(* Main rule, applies each step of preprocessing *)
and start_preprocessing oc = parse
  | "" {
      (* merge of continued lines *)
      let lines_buffer = Buffer.create 2048 in
      splice_lines lines_buffer lexbuf;
      (* comments removal *)
      let clean_buffer = Buffer.create (Buffer.length lines_buffer) in
      remove_comments clean_buffer (lexbuf_of_buffer lines_buffer);
      (* preprocessing *)
      preprocess oc (lexbuf_of_buffer clean_buffer)
    }
