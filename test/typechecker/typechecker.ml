open Libminic

let check_file file =
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let ast = Minic_parser.program Minic_lexer.token lexbuf in
    close_in in_channel;
    Minic_typechecker.typecheck_program ast;
    Printf.printf "Successfuly checked program %s\n" file;
    0
  with
  | Failure s -> Printf.printf "Test failed for program '%s': %s\n" file s; 1
  | _ -> Printf.printf "An unknown exception was raised while ckeking %s\n" file; 1

let test () =
  Sys.readdir "."
  |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".mnc")
  |> List.sort compare
  |> List.fold_left (fun (failed, total) prog -> failed + (check_file prog), total + 1) (0, 0)

let () =
  let failed, total = test () in
  if failed > 0 then begin
    Printf.printf "%d checks failed out of %d, %d passed\n" failed total (total - failed);
    exit 1
  end
  else begin
    Printf.printf "All %d checks passed !\n" total;
    exit 0
  end