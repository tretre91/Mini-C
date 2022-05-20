open Printf
open Wasm

(** Affiche une liste de chaînes de caractère *)
let print_string_list sep channel l =
  let rec print_string_list l =
    match l with
    | [] -> ()
    | [e] -> output_string channel e
    | hd::tl ->
      fprintf channel "%s%s" hd sep; 
      print_string_list tl
  in
  print_string_list l

(* Instructions de gestion des variables *)
let global_get v = Instr ["global.get"; (sprintf "$%s" v)]
let global_set v = Instr ["global.set"; (sprintf "$%s" v)]

let local_get i = Instr ["local.get"; (string_of_int i)]
let local_set i = Instr ["local.set"; (string_of_int i)]
let local_tee i = Instr ["local.tee"; (string_of_int i)]

(* Instructions mémoire *)
let load dtype = Instr [sprintf "%s.load" (string_of_typ dtype)]
let store dtype = Instr [sprintf "%s.store" (string_of_typ dtype)]

(* Instructions numériques *)
let i32_const i = Instr ["i32.const"; (string_of_int i)]

let add dtype = Instr [sprintf "%s.add" (string_of_typ dtype)]
let sub dtype = Instr [sprintf "%s.sub" (string_of_typ dtype)]
let mul dtype = Instr [sprintf "%s.mul" (string_of_typ dtype)]
let div dtype = Instr [sprintf "%s.div_s" (string_of_typ dtype)]
let rem dtype = Instr [sprintf "%s.rem_s" (string_of_typ dtype)]


let eq dtype = Instr [sprintf "%s.eq" (string_of_typ dtype)]
let eqz dtype = Instr [sprintf "%s.eqz" (string_of_typ dtype)]
let neq dtype = Instr [sprintf "%s.ne" (string_of_typ dtype)]
let lt dtype = Instr [sprintf "%s.lt_s" (string_of_typ dtype)]
let le dtype = Instr [sprintf "%s.le_s" (string_of_typ dtype)]
let gt dtype = Instr [sprintf "%s.gt_s" (string_of_typ dtype)]
let ge dtype = Instr [sprintf "%s.ge_s" (string_of_typ dtype)]

let band dtype = Instr [sprintf "%s.and" (string_of_typ dtype)]
let bor dtype = Instr [sprintf "%s.or" (string_of_typ dtype)]
let bxor dtype = Instr [sprintf "%s.xor" (string_of_typ dtype)]

let shl dtype = Instr [sprintf "%s.shl" (string_of_typ dtype)]
let shr_s dtype = Instr [sprintf "%s.shr_s" (string_of_typ dtype)]

(* Instructions de branchement *)

let call f = Instr ["call"; (sprintf "$%s" f)]
let return = Instr ["return"]
let br i = Instr ["br"; string_of_int i]
let br_if i = Instr ["br_if"; string_of_int i]

(* Structures de contrôle *)
let loop body = Loop body

let block body = Block body

let if_then_else s1 s2 = If (s1, s2)

let while_loop cond s =
  Block [
    Loop (
        cond
      @ [eqz I32; br_if 1]
      @ s
      @ [br 0]
    )
  ]

let comment s = Comment s

(** Définition de fonction *)
let func name params locals res body =
  Function (name, params, res, locals, body)

let default_instr = function
  | I32 -> [i32_const 0]
  | _ -> failwith "TODO"

(** Traduction d'un programme *)
let tr_prog prog =
  let tr_fdef fdef =
    let nb_params = List.length Llir.(fdef.params) in
    let get_var = function
      | Llir.Local i -> local_get (i + nb_params)
      | Llir.Param i -> local_get i
      | Llir.Global v -> global_get v
    in
    let set_var = function
      | Llir.Local i -> local_set (i + nb_params)
      | Llir.Param i -> local_set i
      | Llir.Global v -> global_set v
    in
    let tr_num_op = Llir.(function
      | Add -> add I32
      | Sub -> sub I32
      | Mul -> mul I32
      | Div -> div I32
      | Mod -> rem I32
      | Eq  -> eq I32
      | Eqz -> eqz I32
      | Neq -> neq I32
      | Lt  -> lt I32
      | Leq -> le I32
      | Gt  -> gt I32
      | Geq -> ge I32
      | And | BAnd -> band I32
      | Or | BOr -> bor I32
      | Not -> eqz I32
      | BXor -> bxor I32
      | Lsl -> shl I32
      | Asr -> shr_s I32
    ) in
    let rec tr_instr = function
      | Llir.Cst i -> i32_const i
      | Llir.Op op -> tr_num_op op
      | Get v -> get_var v
      | Set v -> set_var v
      | Load dtype -> load dtype
      | Store dtype -> store dtype
      | If (s1, s2) -> if_then_else (tr_seq s1) (tr_seq s2)
      | While (cond, seq) -> while_loop (tr_seq cond) (tr_seq seq)
      | Call f -> call f
      | Return -> return
      | Putchar -> failwith "TODO"
    and tr_seq seq =
      List.map tr_instr seq
    in
    let body = tr_seq fdef.code in
    func fdef.name fdef.params fdef.locals fdef.return body
  in
  let globals = List.map (fun (v, t) -> Global (v, Mut, t, default_instr t)) Llir.(prog.globals) in
  Module (Start "__init" :: Memory 1 :: globals @ (List.map tr_fdef Llir.(prog.functions)))

(** Affichage d'un programme *)
let print_prog channel p =
  let indent = ref 0 in
  (* Version de printf affichant une chaîne indentée *)
  let printfi format =
    output_string channel (String.make !indent ' ');
    fprintf channel format
  in
  (* Affiche une séquence d'expressions *)
  let rec print_seq indent_incr seq =
    indent := !indent + indent_incr;
    List.iter print_expr seq;
    indent := !indent - indent_incr
  (* Affiche une expression *)
  and print_expr = function
    (* Affichage d'une instruction *)
    | Instr atoms ->
      printfi "%a\n" (print_string_list " ") atoms;
    (* Affichage d'un module *)
    | Module seq ->
      printfi "(module\n";
      print_seq 2 seq;
      printfi ")\n"
    (* Affichage d'un bloc *)
    | Block seq ->
      printfi "(block\n";
      print_seq 2 seq;
      printfi ")\n"
    (* Affichage d'une boucle *)
    | Loop seq ->
      printfi "(loop\n";
      print_seq 2 seq;
      printfi ")\n"
    (* Affichage d'un if *)
    | If (s1, s2) ->
      printfi "(if\n";
      printfi "  (then\n";
      print_seq 4 s1;
      printfi "  )\n";
      printfi "  (else\n";
      print_seq 4 s2;
      printfi "  )\n";
      printfi ")\n"
    (* Affichage d'une déclaration de variable globale *)
    | Global (name, q, t, seq) ->
      printfi "(global $%s (%s %s)\n" name (string_of_qualifier q) (string_of_typ t);
      print_seq 2 seq;
      printfi ")\n"
    (* Affichage d'une déclaration de fonction *)
    | Function (name, params, result, locals, seq) ->
      let print_params channel params =
        List.iter (fun typ -> fprintf channel " (param %s)" (string_of_typ typ)) params
      in
      let print_result channel result =
        if Option.is_some result then
          fprintf channel "(result %s)" (string_of_typ (Option.get result))
      in
      printfi "(func $%s (export \"%s\")%a %a\n" name name print_params params print_result result;
      (* Affichage des déclarations de variables locales *)
      if locals <> [] then
        printfi "  (local %a)\n" (print_string_list " ") (List.map string_of_typ locals);
      print_seq 2 seq;
      printfi ")\n"
    (* Affichage d'une clause start *)
    | Start f -> printfi "(start $%s)\n" f
    (* Affichage d'une clause memory *)
    | Memory i -> printfi "(memory %d)\n" i
    (* Affichage d'un commentaire *)
    | Comment c -> printfi ";; %s" c
  in
  print_expr p
