open Libminic

let interpret_file file display =
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  Minic_typechecker.strict_check ast;
  Minic_typechecker.typecheck_program ast;
  let retval = Minic_interpreter.interpret_program ast in
  Printf.printf "\nreturned value: %d\n\n" retval;
  if display then begin
    let out_channel = open_out (file ^ ".copy") in
    Minic_display.print_program ast out_channel;
    close_out out_channel
  end

let test () =
  let suffix, display =
    if Sys.argv.(1) = "step1" then
      ".mnc", true
    else
      ".copy", false
  in
  Sys.readdir "."
  |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file suffix)
  |> List.sort compare
  |> List.iter (fun f -> interpret_file f display)

let () =
  test ()
