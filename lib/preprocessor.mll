{

  open Compile_args

  (** Ecrit le contenu d'un fichier dans un channel *)
  let rec write ic oc =
    try
      let s = input_line ic in
      output_string oc s;
      output_char oc '\n';
      write ic oc
    with
      End_of_file -> close_in ic
  
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

  (** Quitte le programme et affiche un message d'erreur indiquant l'emplacement de l'erreur *)
  let error message pos =
    let open Lexing in
    Printf.fprintf stderr "error at (%d, %d): %s\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) message;
    exit 1

  (** Table de hachage contenant les macros définies *)
  let defines = Hashtbl.create 16

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

let comment = "//"[^'\n']*

rule preprocess args oc = parse
  | word as w {
      let s = match Hashtbl.find_opt defines w with
      | None -> w
      | Some s -> s
      in
      output_string oc s;
      preprocess args oc lexbuf
    }
  | comment {
      preprocess args oc lexbuf
    }
  | "/*" {
    multiline_comment (Lexing.lexeme_start_p lexbuf) lexbuf;
    preprocess args oc lexbuf
  }
  | "#define" space+ (id as id) (space+ (string as replacement) | (space*)) '\n' {
      let pipe_ic, pipe_oc = open_pipe () in
      let buf = Lexing.from_string (Option.value replacement ~default:"") in
      preprocess args pipe_oc buf;
      close_out pipe_oc;
      let replacement' = get_channel_contents pipe_ic in
      Hashtbl.replace defines id (replacement');
      close_in pipe_ic;
      preprocess args oc lexbuf
    }
  | "#undef" space+ (id as id) '\n' {
      Hashtbl.remove defines id;
      preprocess args oc lexbuf
    }
  | "#include" space+ '"' (string as file) '"' '\n' {
      let filename =
        match search_include ((Sys.getcwd ()) :: args.include_paths) file with
        | Some path -> path
        | None -> match search_include [system_include_folder] file with
          | Some path -> path
          | None -> failwith (Printf.sprintf "No file named %s found in the include directories" file)
      in
      let ic = open_in filename in
      let buf = Lexing.from_channel ic in
      preprocess args oc buf;
      close_in ic;
      preprocess args oc lexbuf
    }
  | "#include" space+ '<' (string as file) '>' '\n' {
      let filename =
        match search_include [system_include_folder] file with
        | Some path -> path
        | None -> match search_include ((Sys.getcwd ()) :: args.include_paths) file with
          | Some path -> path
          | None -> failwith (Printf.sprintf "No file named %s found in the include directories" file)
      in
      let ic = open_in filename in
      let buf = Lexing.from_channel ic in
      preprocess args oc buf;
      close_in ic;
      preprocess args oc lexbuf
    }
  | "#ifdef" space+ (condition as cond) space* '\n' {
      let cond = get_condition cond in
      if cond then begin
        let pipe_ic, pipe_oc = open_pipe () in
        ifdef cond 0 pipe_oc lexbuf;
        close_out pipe_oc;
        let code = get_channel_contents pipe_ic in
        close_in pipe_ic;
        preprocess args oc (Lexing.from_string code)
        end
      else
        let oc = open_out Filename.null in
        ifdef cond 0 oc lexbuf;
      preprocess args oc lexbuf
    }
  | "#ifndef" space+ (condition as cond) space* '\n' {
      let cond = not (get_condition cond) in
      if cond then begin
        let pipe_ic, pipe_oc = open_pipe () in
        ifdef cond 0 pipe_oc lexbuf;
        close_out pipe_oc;
        let code = get_channel_contents pipe_ic in
        close_in pipe_ic;
        preprocess args oc (Lexing.from_string code)
        end
      else
        let oc = open_out Filename.null in
        ifdef cond 0 oc lexbuf;
      preprocess args oc lexbuf
    }
  | eof { () }
  | _ as c {
      output_char oc c;
      preprocess args oc lexbuf
    }
(* règle d'analyse des commentaires en blocs (/* ... */) *)
and multiline_comment start_pos = parse
  | ['\n']
      { Lexing.new_line lexbuf; multiline_comment start_pos lexbuf }
  | "*/"
      { () }
  | _
      { multiline_comment start_pos lexbuf }
  | eof
      { error "Forgot to close this multiline comment" start_pos }
(* règle d'analyse des blocs de compilation conditionnelle *)
and ifdef cond depth oc = parse
  | comment {
      ifdef cond depth oc lexbuf
    }
  | "/*" {
    multiline_comment (Lexing.lexeme_start_p lexbuf) lexbuf;
    ifdef cond depth oc lexbuf
  }
  | "#ifdef" space+ (condition as cond') space* '\n' {
    Lexing.new_line lexbuf;
    let s = cond' in
      let cond' = cond && get_condition cond' in
      print_endline ("nested ifdef " ^ s);
      ifdef cond' (depth + 1) oc lexbuf;
      print_endline ("parsed nested ifdef " ^ s);
      (* if cond then
        ifdef cond' (depth + 1) oc lexbuf
      else
        let oc = open_out Filename.null in
        ifdef cond' (depth + 1) oc lexbuf; *)
        Printf.printf "Parsing ifdef at depth %d with cond %b\n" depth cond;
        flush stdout;
      ifdef cond depth oc lexbuf
    }
  | "#ifndef" space+ (condition as cond') space* '\n' {
      Lexing.new_line lexbuf;
      let cond' = cond && (not (get_condition cond')) in
      ifdef cond' (depth + 1) oc lexbuf;
      (* if cond then
        ifdef cond' (depth + 1) oc lexbuf
      else
        let oc = open_out Filename.null in
        ifdef cond' (depth + 1) oc lexbuf; *)
      ifdef cond depth oc lexbuf
    }
  | "#else" space* '\n' {
    Lexing.new_line lexbuf;
    print_endline "else";
      ifdef (not cond) depth oc lexbuf
      (* let cond' = not cond in
      if cond then
        ifdef cond' depth oc lexbuf
      else
        let oc = open_out Filename.null in
        ifdef cond' depth oc lexbuf *)
    }
  | "#endif" _* '\n' {
    Lexing.new_line lexbuf;
    print_endline "endif";
      ()
    }
  | eof {
      (* if depth = 0 then
        ()
      else *)
        error "Missing an endif preprocessor directive" (Lexing.lexeme_start_p lexbuf)
    }
  | _ as c {
    if c= '\n' then Lexing.new_line lexbuf;
      if cond then
        output_char oc c;
      ifdef cond depth oc lexbuf
    }
