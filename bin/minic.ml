open Libminic

let parse_args () =
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [options] <file>\n\nAvailable options are:" in
  let file = ref "" in
  let interpret = ref false in
  let strict = ref true in

  let speclist =
    [("-i", Arg.Set interpret, " Interpret the program");
     ("-lax", Arg.Clear strict, " Allow redefinition of variables and function calls in a global variable's initialization")]
  in
  Arg.parse (Arg.align speclist) (fun f -> if !file = "" then file := f) usage;
  !file, !interpret, !strict

let () =
  let file, interpret, strict = parse_args () in
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  if strict then
    Minic_typechecker.strict_check ast;
  Minic_typechecker.typecheck_program ast;
  Printf.printf "Successfully checked program %s\n" file;
  if interpret then
    exit (Minic_interpreter.interpret_program ast)
  else
    exit 0
  (* On pourrait ajouter ici des étapes suivantes.
  exit 0 *)
