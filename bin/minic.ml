open Libminic.Compiler
open Libminic.Compile_args
open Cmdliner

(** Fonction principale *)
let main input_file output_file include_paths defines dump_pp gc =
  let definitions =
    if gc
    then ("__MINIC_GC", None) :: defines
    else defines
  in
  let args = {
    input_file;
    output_file;
    include_paths;
    definitions;
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
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"SOURCE" ~doc)
  in
  let include_paths =
    let doc = "Add a directory to the include path" in
    Arg.(value & opt_all dir [] & info ["I"] ~docv:"PATH" ~doc)
  in
  let defines =
    let define_conv =
      let parse s =
        let v = match String.index_opt s '=' with
        | None -> s, None
        | Some i -> String.sub s 0 i, Some (String.sub s (i + 1) ((String.length s) - i - 1))
        in
        Ok v
      in
      let print ppf (macro, value) =
        let value = match value with
        | None -> ""
        | Some v -> Printf.sprintf "=%s" v
        in
        Format.fprintf ppf "%s%s" macro value
      in
      Arg.conv' (parse, print)
    in
    let doc = "Add a macro definition" in
    Arg.(value & opt_all define_conv [] & info ["D"] ~docv:"NAME[=VALUE]" ~doc)
  in
  let dump_pp =
    let doc = "Output the preprocessed sources to $(docv)" in
    Arg.(value & opt ~vopt:(Some "a.out.pp") (some string) None & info ["dump"] ~docv:"FILE" ~doc)
  in
  let enable_gc =
    let doc = "Enable a reference counting garbage collector (wip)" in
    Arg.(value & flag & info ["gc"] ~doc)
  in
  let args_t = Term.(const main $ input_file $ output_file $ include_paths $ defines $ dump_pp $ enable_gc) in
  let doc = "Compiler targeting WebAssembly" in
  let info = Cmd.info "minic" ~version:"0.0.1" ~doc in
  Cmd.v info args_t

let () =
  exit (Cmd.eval command_line)