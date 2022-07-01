open Printf
open Libminic
open Libminic.Compile_args

(** Execute une commande et lève une exception si la valeur de retour est non nulle *)
let execute_command command =
  let exit_code = Sys.command command in
  if exit_code <> 0 then
    failwith (sprintf "Command \"%s\" returned %d" command exit_code)

(** Compile un fichier minic avec gcc puis l'execute et renvoie sa sortie vers
    un fichier *)
let gcc_compile_exec filename =
  let source_file = Filename.temp_file "minic_tests_source" ".c" in
  let output_file = Filename.temp_file "minic_tests_output_gcc" ".txt" in
  let executable = Filename.concat (Filename.get_temp_dir_name ()) "test" in

  let cat_src_cmd = Filename.quote_command "cat" [filename] in
  let compile_cmd = Filename.quote_command "gcc" [source_file; "-o"; executable; "-I"; Sys.getcwd ()] in
  
  execute_command (sprintf "%s >> %s" cat_src_cmd source_file);
  execute_command compile_cmd;
  begin try
    execute_command (sprintf "%s > %s" executable output_file)
  with
    Failure _ -> ()
  end;

  output_file

(** Compile un fichier minic, le traduit en wasm puis l'execute et renvoie sa
    sortie vers un fichier *)
let minic_compile_exec filename =
  Hashtbl.clear Preprocessor.defines;
  Minic_lexer.reset_strings ();
  let wat_file = Filename.temp_file "minic_tests_file" ".wat" in
  let args = {
    input_file = filename;
    output_file = Some wat_file;
    include_paths = [Sys.getcwd ()];
    definitions = [];
    dump_preproc = None;
  }
  in
  args
  |> Compiler.preprocess
  |> Compiler.create_ast
  |> Compiler.compile args;

  let wasm_file = Filename.temp_file "minic_tests_file" ".wasm" in
  let output_file = Filename.temp_file "minic_tests_output_mnc" ".txt" in
  let wasm_compile_cmd = Filename.quote_command "wat2wasm" [wat_file; "-o"; wasm_file] in
  let wasm_runtime_cmd = Filename.quote_command "deno" ["run"; "--allow-read"; "--quiet"; "../runtime/runtime.mts"; wasm_file] in

  execute_command wasm_compile_cmd;
  execute_command (sprintf "%s > %s" wasm_runtime_cmd output_file);

  output_file

(** Compare le contenu de deux fichiers, lève une exception si ils sont différents *)
let diff_test file1 file2 =
  let config = Patdiff.Configuration.default in
  match Patdiff.Compare_core.diff_files config ~prev_file:file1 ~next_file:file2 with
  | `Same -> ()
  | `Different -> failwith ""

(** Compile un fichier avec gcc et minic et compare le résultat de l'execution *)
let test filename =
  try
    let gcc_output = gcc_compile_exec filename in
    let mnc_output = minic_compile_exec filename in
    diff_test gcc_output mnc_output;
    Printf.printf "test passed for file %s\n" filename
  with
    _ as e ->
      Printf.printf "test failed for file %s\nexecption is: %s\n" filename (Printexc.to_string e)

let main () =
  Sys.readdir "."
  |> Array.to_seq
  |> Seq.filter (fun filename -> Filename.extension filename = ".c")
  |> List.of_seq
  |> List.sort compare
  |> List.iter test

let () = main ()
