open Libminic
open Cmdliner

type args = {
  input_file: string;
  output_file: string option;
}

(** Compile un prgramme minic en module WebAssembly *)
let compile ast args =
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

(** Fonction principale *)
let main input_file output_file =
  let args = {
    input_file;
    output_file;
  }
  in
  let in_channel = open_in args.input_file in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  let typed_ast = Minic_typechecker.typecheck_program ast in
  compile typed_ast args

let command_line =
  let output_file =
    let doc = "Place the output into $(docv)" in
    Arg.(value & opt (some string) None & info ["o"] ~docv:"FILE" ~absent:"Outputs to stdout" ~doc)
  in
  let input_file =
    let doc = "The source file" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"SOURCE" ~doc)
  in
  let args_t = Term.(const main $ input_file $ output_file) in
  let doc = "Compiler targeting WebAssembly !" in
  let info = Cmd.info "minic" ~version:"0.0.1" ~doc in
  Cmd.v info args_t

let () =
  exit (Cmd.eval command_line)