open Libminic.Compiler
open Libminic.Compile_args
open Cmdliner

(** Fonction principale *)
let main input_file output_file include_paths dump_pp =
  let args = {
    input_file;
    output_file;
    include_paths;
    dump_preproc = dump_pp;
  }
  in
  let preprocessed_file = preprocess args in
  let typed_ast = create_ast preprocessed_file in
  compile args typed_ast

let command_line =
  let output_file =
    let doc = "Place the output into $(docv)" in
    Arg.(value & opt (some string) None & info ["o"] ~docv:"FILE" ~absent:"Outputs to stdout" ~doc)
  in
  let input_file =
    let doc = "The source file" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"SOURCE" ~doc)
  in
  let include_paths =
    let doc = "The directories to search for include files" in
    Arg.(value & opt_all string [] & info ["I"] ~docv:"PATH" ~doc)
  in
  let dump_pp =
    let doc = "Output the preprocessed sources to $(docv)" in
    Arg.(value & opt ~vopt:(Some "a.out.pp") (some string) None & info ["dump"] ~docv:"FILE" ~doc)
  in
  let args_t = Term.(const main $ input_file $ output_file $ include_paths $ dump_pp) in
  let doc = "Compiler targeting WebAssembly !" in
  let info = Cmd.info "minic" ~version:"0.0.1" ~doc in
  Cmd.v info args_t

let () =
  exit (Cmd.eval command_line)