open Printf
open Sexplib.Sexp

let pair a b = List [Atom a; Atom b]

(* Instructions de gestion des variables *)
let global_get v = pair "global.get" (sprintf "$%s" v)
let global_set v = pair "global.set" (sprintf "$%s" v)

let local_get i = pair "local.get" (string_of_int i)
let local_set i = pair "local.set" (string_of_int i)
let local_tee i = pair "local.tee" (string_of_int i)

(* Instructions numériques *)
let i32_const i = pair "i32.const" (string_of_int i)
let i32_add = Atom ("i32.add")
let i32_mul = Atom ("i32.mul")
let i32_lt = Atom ("i32.lt_s")

let i32_eqz = Atom ("i32.eqz")

(* Instructions de branchement *)

let call f = pair "call" (sprintf "$%s" f)
let return = Atom ("return")
let br i = pair "br" (string_of_int i)
let br_if i = pair "br_if" (string_of_int i)

let loop body =
  List (Atom "loop" :: body)

let block body =
  List (Atom "block" :: body)

let if_then_else s1 s2 =
  List [
    Atom "if";
    List (Atom "then" :: s1);
    List (Atom "else" :: s2)
  ]

let while_loop cond s =
  List [
    Atom "block";
    List (Atom "loop" :: (cond @ [i32_eqz; br_if 1] @ s @ [br 0]))
  ]

let comment s = Atom (";;" ^ s)

(** Définition de fonction *)
let func name nb_params nb_locals res body =
  List (
    (* TODO : problème de double quote lors du print de la sexp export *)
    Atom "func" :: Atom (sprintf "$%s" name)  :: (List [Atom "export"; Atom (sprintf "\"%s\"" name)]) ::
    (List.init nb_params (fun _ -> pair "param" "i32"))
    @ [pair "result" "i32"]
    @ List.init nb_locals (fun _ -> pair "local" "i32")
    @ body
  )

(** Traduction d'un programme *)
let tr_prog prog =
  let tr_fdef fdef =
    let get_var = function
      | Llir.Local i -> local_get (i + Llir.(fdef.nb_params))
      | Llir.Param i -> local_get i
      | Llir.Global v -> global_get v
    in
    let set_var = function
      | Llir.Local i -> local_set (i + Llir.(fdef.nb_params))
      | Llir.Param i -> local_set i
      | Llir.Global v -> global_set v
    in
    let rec tr_instr = function
      | Llir.Cst i -> i32_const i
      | Llir.Add -> i32_add
      | Llir.Mul -> i32_mul
      | Llir.Lt -> i32_lt
      | Get v -> get_var v
      | Set v -> set_var v
      | If (s1, s2) -> if_then_else (tr_seq s1) (tr_seq s2)
      | While (cond, seq) -> while_loop (tr_seq cond) (tr_seq seq)
      | Call f -> call f
      | Return -> return
      | Putchar -> failwith "TODO"
    and tr_seq seq =
      List.map tr_instr seq
    in
    let body = tr_seq fdef.code in
    func fdef.name fdef.nb_params fdef.nb_locals () body
  in
  let globals = List.map (fun v -> List [Atom "global"; Atom (sprintf "$%s" v); pair "mut" "i32"; i32_const 0]) Llir.(prog.globals) in
  List (Atom "module" :: globals @ (List.map tr_fdef Llir.(prog.functions)))

(** Affichage d'un programme *)
let print_prog channel p =
  Sexplib.Sexp.default_indent := 2;
  output_string channel (Sexplib.Sexp.to_string p);
  output_char channel '\n'
