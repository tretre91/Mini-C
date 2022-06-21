open Compile_args

let preprocess args =
  let filename, oc =
    match args.dump_preproc with
    | None -> Filename.open_temp_file "minic_preproc" ".pp"
    | Some file -> file, open_out file
  in
  let ic = open_in args.input_file in
  let lexbuf = Lexing.from_channel ic in
  Preprocessor.preprocess args oc lexbuf;
  close_in ic;
  close_out oc;
  filename

let create_ast filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  Minic_typechecker.typecheck_program ast

(** Compile un prgramme minic en module WebAssembly *)
let compile args ast =
  let file, close = match args.output_file with
    | None -> stdout, fun _ -> ()
    | Some f -> open_out f, close_out
  in
  ast
  |> Minic.prog_of_ast
  |> Minic_llir.tr_prog
  |> Minic_wasm.tr_prog
  |> Minic_wasm.print_prog file;
  close file