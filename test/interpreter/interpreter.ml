open Libminic

let interpret_file file =
  Printf.printf "**%s**\n" file;
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let ast = Minic_parser.program Minic_lexer.token lexbuf in
    close_in in_channel;
    Minic_typechecker.typecheck_program ast;
    let retval = Minic_interpreter.interpret_program ast in
    Printf.printf "\nreturned value: %d\n\n" retval
  with
  | Failure s -> failwith (Printf.sprintf "Failed to typecheck or interpret file '%s': %s\n" file s)
  | e -> Printf.printf "An unknown exception was raised while ckeking '%s'\n" file; raise e

let test () =
  Sys.readdir "."
  |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".mnc")
  |> List.sort compare
  |> List.iter interpret_file

let () =
  test ()