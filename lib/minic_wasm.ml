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
let load dtype =
  let instr = match dtype with
    | I8  -> "i32.load8_s"
    | I16 -> "i32.load16_s"
    | I32 -> "i32.load"
    | I64 -> "i64.load"
    | F32 -> "f32.load"
    | F64 -> "f64.load"
  in
  Instr [instr]

let store dtype =
  let instr = match dtype with
    | I8  -> "i32.store8"
    | I16 -> "i32.store16"
    | I32 -> "i32.store"
    | I64 -> "i64.store"
    | F32 -> "f32.store"
    | F64 -> "f64.store"
  in
  Instr [instr]

let mem_init id = Instr ["memory.init"; string_of_int id]

(* Instructions numériques *)
let const = function
  | Llir.I32Cst i -> Instr ["i32.const"; Int32.to_string i]
  | Llir.I64Cst i -> Instr ["i64.const"; Int64.to_string i]
  | Llir.F32Cst f -> Instr ["f32.const"; string_of_float f]
  | Llir.F64Cst f -> Instr ["f64.const"; string_of_float f]

let cast from t =
  let instr = Llir.(match from, t with
  | Int8, (Int16 | Int32) -> ["i32.extend8_s"]
  | Int8, Int64 -> ["i64.extend8_s"]
  | Int8, Float32 -> ["i32.extend8_s\nf32.convert_i32_s"] (* TODO :( *)
  | Int8, Float64 -> ["i32.extend8_s\nf64.convert_i32_s"]
  | Int16, Int32 -> ["i32.extend16_s"]
  | Int16, Int64 -> ["i64.extend16_s"]
  | Int16, Float32 -> ["i32.extend16_s\nf32.convert_i32_s"] (* TODO :( *)
  | Int16, Float64 -> ["i32.extend16_s\nf64.convert_i32_s"]
  | Int32, Int8 -> []
  | Int32, Int16 -> []
  | Int32, Int64 -> ["i64.extend_i32_s"]
  | Int32, Float32 -> ["f32.convert_i32_s"]
  | Int32, Float64 -> ["f64.convert_i32_s"]
  | Int64, (Int8 | Int16 | Int32) -> ["i32.wrap_i64"]
  | Int64, Float32 -> ["f32.convert_i64_s"]
  | Int64, Float64 -> ["f64.convert_i64_s"]
  | Float32, Int8 -> failwith __LOC__
  | Float32, Int16 -> failwith __LOC__
  | Float32, Int32 -> ["i32.trunc_f32_s"]
  | Float32, Int64 -> ["i64.trunc_f32_s"]
  | Float32, Float64 -> ["f64.promote_f32"]
  | Float64, Int8 -> failwith __LOC__
  | Float64, Int16 -> failwith __LOC__
  | Float64, Int32 -> ["i32.trunc_f64_s"]
  | Float64, Int64 -> ["i64.trunc_f64_s"]
  | Float64, Float32 -> ["f32.demote_f64"]
  | _, _ when from = t -> []
  | _, _ -> failwith "unreachable"
  ) in
  Instr instr

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

let drop = Instr ["drop"]

let comment s = Comment s

(** Définition de fonction *)
let func name params locals res body =
  Function (name, params, res, locals, body)

let default_instr = function
  | I32 -> [const (Llir.I32Cst 0l)]
  | I64 -> [const (Llir.I64Cst 0L)]
  | F32 -> [const (Llir.F32Cst 0.0)]
  | F64 -> [const (Llir.F64Cst 0.0)]
  | _   -> failwith __LOC__ (* unreachable *)

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
      | Add -> add
      | Sub -> sub
      | Mul -> mul
      | Div -> div
      | Mod -> rem
      | Eq  -> eq
      | Eqz -> eqz
      | Neq -> neq
      | Lt  -> lt
      | Leq -> le
      | Gt  -> gt
      | Geq -> ge
      | And | BAnd -> band
      | Or | BOr -> bor
      | Not -> eqz
      | BXor -> bxor
      | Lsl -> shl
      | Asr -> shr_s
    ) in
    let rec tr_instr : (Llir.instr -> Wasm.expr) = function
      | Cst i -> const i
      | Cast (from, to_) -> cast from to_
      | Op (dt, op) -> (tr_num_op op) dt
      | Get v -> get_var v
      | Set v -> set_var v
      | Load dtype -> load dtype
      | Store dtype -> store dtype
      | MemInit id -> mem_init id
      | If (s1, s2) -> if_then_else (tr_seq s1) (tr_seq s2)
      | While (cond, seq) -> while_loop (tr_seq cond) (tr_seq seq)
      | Call f -> call f
      | Return -> return
      | Drop -> drop
    and tr_seq seq =
      List.map tr_instr seq
    in
    let body = tr_seq fdef.code in
    func fdef.name fdef.params fdef.locals fdef.return body
  in
  let tr_extern_func (fdef: Llir.fun_def) =
    ImportedFunction (fdef.name, fdef.params, fdef.return)
  in
  let globals = List.map (fun (v, t) -> Global (v, Mut, t, default_instr t)) Llir.(prog.globals) in
  let data = List.map (fun (addr, data) -> Data (addr, data)) Llir.(prog.static) in
  Module (
       Start "__init"
    :: (List.map tr_extern_func Llir.(prog.extern_functions))
    @ [Memory 2;
       Global ("__sp", Mut, I32, [const (Llir.I32Cst 65536l)])]
    @  data
    @  globals
    @  (List.map tr_fdef Llir.(prog.functions))
  )

(** Affichage d'un programme *)
let print_prog channel p =
  let indent = ref 0 in
  (* Version de printf affichant une chaîne indentée *)
  let printfi format =
    output_string channel (String.make !indent ' ');
    fprintf channel format
  in
  (* Affichage des paramètres d'une fonction *)
  let print_params channel params =
    List.iter (fun typ -> fprintf channel " (param %s)" (string_of_typ typ)) params
  in
  (* Affichage du type de retour d'une fonction *)
  let print_result channel result =
    if Option.is_some result then
      fprintf channel "(result %s)" (string_of_typ (Option.get result))
  in
  (* Affiche une séquence d'expressions *)
  let rec print_seq indent_incr seq =
    indent := !indent + indent_incr;
    List.iter print_expr seq;
    indent := !indent - indent_incr
  (* Affiche une expression *)
  and print_expr = function
    (* Affichage d'une instruction *)
    | Instr [] -> ()
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
    (* Affichage d'une fonction importée *)
    | ImportedFunction (name, params, result) ->
      printfi "(import \"std\" \"%s\" (func $%s%a %a))\n" name name print_params params print_result result
    (* Affichage d'une déclaration de fonction *)
    | Function (name, params, result, locals, seq) ->
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
    (* Affichage d'une clause data *)
    | Data (addr, data) ->
      begin match addr with
      | None -> printfi "(data \"%s\")\n" data
      | Some a -> printfi "(data (i32.const %d) \"%s\")\n" a data (* TODO : ne pas hardcoder le i32.const *)
      end
    (* Affichage d'un commentaire *)
    | Comment c -> printfi ";; %s" c
  in
  print_expr p
