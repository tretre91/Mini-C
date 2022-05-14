open Libminic
open Cmdliner

type args = {
  input_file: string;
  interpret: bool;
  display: bool;
  output_file: string option;
}

(** Effectue une vérification de type sur le programme source *)
let typecheck ast args =
  Minic_typechecker.typecheck_program ast;
  Printf.printf "Successfully checked program %s\n" args.input_file

(** Reconstruit et affiche un programme minic reconstruit à partir de l'ast
    d'un programme donné *)
let display_ast ast args =
  match args.output_file with
  | None -> Minic_display.print_program ast stdout
  | Some f -> begin
      let out_channel = open_out f in
      Minic_display.print_program ast out_channel;
      close_out out_channel;
    end

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
let main input_file output_file interpret display =
  let args = {
    input_file;
    interpret;
    display;
    output_file;
  }
  in
  let in_channel = open_in args.input_file in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  typecheck ast args;
  if args.display then
    display_ast ast args
  else if args.interpret then
    exit (Minic_interpreter.interpret_program ast)
  else
    compile ast args

let command_line =
  let output_file =
    let doc = "Place the output into $(docv)" in
    Arg.(value & opt (some string) None & info ["o"] ~docv:"FILE" ~absent:"Outputs to stdout" ~doc)
  in
  let input_file =
    let doc = "The source file" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"SOURCE" ~doc)
  in
  let interpret: bool Term.t =
    let doc = "Interpret the minic program" in
    Arg.(value & flag & info ["i"; "interpret"] ~doc)
  in
  let display =
    let doc = "Reconstruct the source file from the ast" in
    Arg.(value & flag & info ["display"] ~doc)
  in
  let args_t = Term.(const main $ input_file $ output_file $ interpret $ display) in
  let doc = "Compiler targeting WebAssembly !" in
  let info = Cmd.info "minic" ~version:"0.0.1" ~doc in
  Cmd.v info args_t

let () =
  exit (Cmd.eval command_line)