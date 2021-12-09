open Libminic

type args = {
  input_file: string;
  interpret: bool;
  display: bool;
  display_file: string option;
  strict: bool;
}
    
let parse_args () =
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [options] <file> [-d [<file>]]\n\nAvailable options are:" in
  let input_file = ref "" in
  let interpret = ref false in
  let display = ref false in
  let display_file = ref None in
  let strict = ref true in

  let handle_display () =
    display := true;
    try
      let next = Sys.argv.(!Arg.current + 1) in
      if next.[0] <> '-' then
        display_file := Some next
    with
    | _ -> ()
  in  

  let speclist =
    [("-i", Arg.Set interpret, " Interpret the program");
     ("-lax", Arg.Clear strict, " Allow redefinition of variables and function calls in a global variable's initialization");
     ("-d", Arg.Unit handle_display, " Output the source code derived from the AST, the code is displayed on stdout if no argument is given")]
  in
  Arg.parse (Arg.align speclist) (fun f -> if !input_file = "" then input_file := f) usage;
  {
    input_file = !input_file;
    interpret = !interpret;
    display = !display;
    display_file = !display_file;
    strict = !strict;
  }

let typecheck ast args =
  if args.strict then
    Minic_typechecker.strict_check ast;
  Minic_typechecker.typecheck_program ast;
  Printf.printf "Successfully checked program %s\n" args.input_file

let display ast args =
  match args.display_file with
  | None -> Minic_display.print_program ast stdout
  | Some f -> begin
      let out_channel = open_out f in
      Minic_display.print_program ast out_channel;
      close_out out_channel;
    end

let () =
  let args = parse_args () in
  let in_channel = open_in args.input_file in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  typecheck ast args;
  if args.display then
    display ast args;
  if args.interpret then
    exit (Minic_interpreter.interpret_program ast)
  else
    exit 0
  (* On pourrait ajouter ici des étapes suivantes.
  exit 0 *)
