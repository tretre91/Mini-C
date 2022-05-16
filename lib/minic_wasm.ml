open Printf

type typ = I32 | I64 | F32 | F64

type qualifier = Mut | Const

type expr =
  | Instr of string list
  | Module of seq
  | Block of seq
  | Loop of seq
  | If of seq * seq
  | Function of string * typ list * typ option * typ list * seq
  | Global of string * qualifier * typ * seq
  | Comment of string
and seq = expr list

let string_of_typ = function
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"

let string_of_qualifier = function
  | Mut -> "mut"
  | Const -> "const"

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

(* Instructions numériques *)
let i32_const i = Instr ["i32.const"; (string_of_int i)]
let i32_add = Instr ["i32.add"]
let i32_sub = Instr ["i32.sub"]
let i32_mul = Instr ["i32.mul"]
let i32_lt = Instr ["i32.lt_s"]

let i32_eqz = Instr ["i32.eqz"]

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
      @ [i32_eqz; br_if 1]
      @ s
      @ [br 0]
    )
  ]

let comment s = Comment s

(** Définition de fonction *)
let func name nb_params nb_locals ?res body =
  let params = List.init nb_params (fun _ -> I32) in
  let locals = List.init nb_locals (fun _ -> I32) in
  Function (name, params, res, locals, body)

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
      | Llir.Sub -> i32_sub
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
    func fdef.name fdef.nb_params fdef.nb_locals ~res:I32 body
  in
  let globals = List.map (fun v -> Global (v, Mut, I32, [i32_const 0])) Llir.(prog.globals) in
  Module (globals @ (List.map tr_fdef Llir.(prog.functions)))

(** Affichage d'un programme *)
let print_prog channel p =
  let printf format = fprintf channel format in
  let print = output_string channel in
  let rec print_expr indent expr =
    match expr with
    | Instr atoms ->
      print indent;
      print_string_list " " channel atoms;
      print "\n"
    | Module seq ->
      printf "%s(module\n" indent;
      print_seq (indent ^ "  ") seq;
      printf "%s)\n" indent
    | Block seq ->
      printf "%s(block\n" indent;
      print_seq (indent ^ "  ") seq;
      printf "%s)\n" indent
    | Loop seq ->
      printf "%s(loop\n" indent;
      print_seq (indent ^ "  ") seq;
      printf "%s)\n" indent
    | If (s1, s2) ->
      printf "%s(if\n" indent;
      printf "%s  (then\n" indent;
      print_seq (indent ^ "    ") s1;
      printf "%s  )\n" indent;
      printf "%s  (else\n" indent;
      print_seq (indent ^ "    ") s2;
      printf "%s  )\n" indent;
      printf "%s)\n" indent
    | Global (name, q, t, seq) ->
      printf "%s(global $%s (%s %s)\n" indent name (string_of_qualifier q) (string_of_typ t);
      print_seq (indent ^ "  ") seq;
      printf "%s)\n" indent
    | Function (name, params, result, locals, seq) ->
      printf "%s(func $%s (export \"%s\")" indent name name;
      List.iter (fun typ -> printf " (param %s)" (string_of_typ typ)) params;
      if Option.is_some result then
        printf " (result %s)" (string_of_typ (Option.get result));
      print "\n";
      (* Affichage des déclarations de variables locales *)
      if locals <> [] then begin
        printf "%s  (local " indent;
        print_string_list " " channel (List.map string_of_typ locals);
        print ")\n"
      end;
      print_seq (indent ^ "  ") seq;
      printf "%s)\n" indent
    | Comment c -> fprintf channel "%s;; %s" indent c
  and print_seq indent seq =
    List.iter (print_expr indent) seq
  in
  print_expr "" p
