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

}

let space = [' ''\t''\r']
let alpha = ['a'-'z''A'-'Z']
let alnum = ['a'-'z''A'-'Z''0'-'9''_']
let word = alnum+
let id = (alpha | '_')alnum*
let string = [^'#''\n']*

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
      let ifd, ofd = Unix.pipe () in
      let pipe_ic = Unix.in_channel_of_descr ifd in
      let pipe_oc = Unix.out_channel_of_descr ofd in
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
  | eof { () }
  | _ as c {
      output_char oc c;
      preprocess args oc lexbuf
    }
and multiline_comment start_pos = parse
  | ['\n']
      { Lexing.new_line lexbuf; multiline_comment start_pos lexbuf }
  | "*/"
      { () }
  | _
      { multiline_comment start_pos lexbuf }
  | eof
      { error "Forgot to close this multiline comment" start_pos }