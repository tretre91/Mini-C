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
}

let space = [' ''\t''\r']
let alpha = ['a'-'z''A'-'Z']
let alnum = ['a'-'z''A'-'Z''0'-'9''_']
let word = alnum+
let id = (alpha | '_')alnum*
let string = [^'\n']+

rule preprocess args oc = parse
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
