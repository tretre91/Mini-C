%{

  open Lexing
  open Minic_ast

  let make_expr e =
    { t = Mut Void; const = false; expr = e }
  
  (* Valeur par défaut d'une variable d'un type donné *)
  let default_value typ =
    let t = match typ with
      | Mut t -> t
      | Const t -> t
    in
    match t with
    | Integer t -> Cst (CInteger (t, Int64.zero))
    | Float -> Cst (CFloat 0.0)
    | Double -> Cst (CDouble 0.0)
    | Bool -> Cst (CBool false)
    | Void -> Cst (CBool true)      (* peu importe, sera détécté par le vérificateur de type *)
    | Ptr _ -> Cst (CInteger (Int, Int64.zero))
    | Tab (_, _) -> InitList []
  
  (* Traduit une boucle for en boucle while *)
  let for_loop init cond incr body =
    let condition = Option.value cond ~default:(make_expr (Cst (CBool true))) in
    let increment = List.map (fun (id, e) -> Set(id, e)) incr in
    let body = body @ increment in
    Block (init @ [While (condition, body)])
  
%}

(* Déclaration des lexèmes *)
%token <Minic_ast.constant> CST
%token <string> IDENT
%token <string> SCOPED_ATTR
%token EXTERN CONST
%token LPAR RPAR BEGIN END LBRACKET RBRACKET
%token RETURN SET SEMI COMMA
%token IF ELSE WHILE FOR
%token CHAR SHORT INT LONG FLOAT DOUBLE BOOL VOID
%token ADD SUB MUL DIV MOD
%token EQ NEQ
%token LT LEQ GT GEQ
%token AND OR NOT
%token BAND BOR BXOR BNOT
%token LSL ASR
%token EOF

%left OR
%left AND
%left BOR
%left BXOR
%left BAND
%left EQ NEQ
%nonassoc LT LEQ GT GEQ
%left LSL ASR
%left SUB ADD
%left DIV MUL MOD
%nonassoc NOT BNOT

%start program
%type <Minic_ast.prog> program

%%

(* Un programme est une liste de déclarations.
   On ajoute une règle déclenchée en cas d'erreur, donnant une
   information minimale : la position. *)
program:
| dl=list(global_declaration) EOF
       { List.flatten dl }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

(* Une déclaration globale, soit des variables globales soit une fonction *)
global_declaration:
| vars=variable_decl SEMI { List.map (fun v -> Variable v) vars }
| func=function_d { [Function func] }
;

(* Déclaration de variables. *)
variable_decl:
| t=ctyp vars=separated_list(COMMA, id=IDENT e=option(SET e=expression { e }) { id, e })
          { List.map (fun (id, e) -> id, t, Option.value e ~default:(make_expr (default_value t))) vars }
| t=ctyp id=IDENT LBRACKET n=expression RBRACKET l=option(SET BEGIN l=separated_list(COMMA, expression) END { l })
          {
            let l = Option.value ~default:[] l in
            [id, Const (Tab (t, n)), { t; const = false; expr = InitList l }]
          }
| t=ctyp id=IDENT LBRACKET RBRACKET SET BEGIN l=separated_list(COMMA, expression) END
          { 
            let len = make_expr (Cst (CInteger (Int, Int64.of_int (List.length l)))) in
            [id, Const (Tab (t, len)), {t; const = false; expr = InitList l }]
          }
| t=ctyp id=IDENT LBRACKET RBRACKET SET e=expression
          { 
            let len = make_expr (Cst (CInteger (Int, -1L))) in
            [id, Const (Tab (t, len)), e]
          }
;

(* Indication de type. *)
typ:
| CHAR          { Integer Char }
| SHORT         { Integer Short }
| SHORT INT     { Integer Short }
| INT           { Integer Int }
| LONG          { Integer Long }
| LONG INT      { Integer Long }
| LONG LONG     { Integer Long }
| LONG LONG INT { Integer Long }
| FLOAT         { Float }
| DOUBLE        { Double }
| BOOL          { Bool }
| VOID          { Void }
;

ctyp:
| t=typ { Mut t }
| CONST t=typ { Const t }
| t=typ CONST { Const t }
| t=ctyp MUL { Mut (Ptr t) }
| t=ctyp MUL CONST { Const (Ptr t) }
;

(* Déclaration d'attributs *)
attribute:
| attr=IDENT       { attr }
| attr=SCOPED_ATTR { attr }

attributes:
| LBRACKET LBRACKET l=separated_list(COMMA, attribute) RBRACKET RBRACKET { l }
| EXTERN { ["extern"] }
;

(* Déclaration de fonction. *)
function_d:
| func=function_decl { func }
| attr=attributes f=function_d { { f with attributes = attr @ f.attributes } }
;

function_decl:
| t=ctyp f=IDENT LPAR p=separated_list(COMMA, parameter) RPAR b=block
    { { name=f; params=p; return=t; body=b; is_forward_decl=false; attributes=[] } }
| t=ctyp f=IDENT LPAR p=separated_list(COMMA, parameter) RPAR SEMI
    { { name=f; params=p; return=t; body=[]; is_forward_decl=true; attributes=[] } }
;

(* Paramètre formel d'une fonction *)
parameter:
| t=ctyp id=IDENT { id, t }
;

(* Bloc d'instructions *)
block:
| BEGIN i=list(instruction) END { i }
;

(* If *)
if_sequence:
| IF LPAR c=expression RPAR t=block ELSE f=block       { If(c, t, f) }   (* if / else *)
| IF LPAR c=expression RPAR t=block                    { If(c, t, []) }  (* if *)
| IF LPAR c=expression RPAR t=block ELSE i=if_sequence { If(c, t, [i]) } (* if / else if / ... *)
;

(* Accès à une case d'un tableau *)
subscript:
| id=IDENT LBRACKET i=expression RBRACKET { id, i }
;

(* Instructions. *)
instruction:
| v=variable_decl SEMI                           { Decl(v) }
| a=assignement SEMI                             { let id, e = a in Set(id, e) }
| s=subscript SET e=expression SEMI              { let id, i = s in Write (make_expr (Get id), i, e) }
| MUL addr=expression SET e=expression SEMI      { Write (addr, make_expr (Cst (CInteger (Int, 0L))), e) }
| i=if_sequence                                  { i }
| WHILE LPAR c=expression RPAR b=block           { While(c, b) }
| FOR LPAR init=for_init_statement SEMI cond=option(expression) SEMI i=separated_list(COMMA, assignement) RPAR b=block
                                                 { for_loop init cond i b }
| RETURN e=expression SEMI                       { Return(e) }
| e=expression SEMI                              { Expr(e) }
| b=block                                        { Block(b) }
;

(* Initialisation d'une boucle for, peut être de la forme :
   - t x1 = v1, ..., xn = vn (déclare les variables x1...xn, toutes du même type)
   - x1 = v1, ..., xn = vn   (attribue des valeurs à x1...xn, pas forcèment de même type)  *)
for_init_statement:
| v=variable_decl { [Decl v] }
| l=separated_list(COMMA, assignement)
             { List.map (fun (id, e) -> Set(id, e)) l }
;

(* Modification de variable *)
assignement:
| id=IDENT SET e=expression { id, e }
;

(* Expressions. *)
expression:
| LPAR e=expression RPAR            { e }
| c=CST                             { make_expr (Cst(c)) }
| SUB e=expression                  { make_expr (UnaryOperator(Minus, e)) }
| ADD e=expression                  { e }
| NOT e=expression                  { make_expr (UnaryOperator(Not, e)) }
| BNOT e=expression                 { make_expr (UnaryOperator(BNot, e)) }
| MUL e=expression                  { make_expr (Read(e, make_expr (Cst (CInteger (Int, 0L))))) }
| e1=expression ADD e2=expression   { make_expr (BinaryOperator(Add, e1, e2)) }
| e1=expression SUB e2=expression   { make_expr (BinaryOperator(Sub, e1, e2)) }
| e1=expression MUL e2=expression   { make_expr (BinaryOperator(Mult, e1, e2)) }
| e1=expression DIV e2=expression   { make_expr (BinaryOperator(Div, e1, e2)) }
| e1=expression MOD e2=expression   { make_expr (BinaryOperator(Mod, e1, e2)) }
| e1=expression EQ e2=expression    { make_expr (BinaryOperator(Eq, e1, e2)) }
| e1=expression NEQ e2=expression   { make_expr (BinaryOperator(Neq, e1, e2)) }
| e1=expression LT e2=expression    { make_expr (BinaryOperator(Lt, e1, e2)) }
| e1=expression LEQ e2=expression   { make_expr (BinaryOperator(Leq, e1, e2)) }
| e1=expression GT e2=expression    { make_expr (BinaryOperator(Gt, e1, e2)) }
| e1=expression GEQ e2=expression   { make_expr (BinaryOperator(Geq, e1, e2)) }
| e1=expression AND e2=expression   { make_expr (BinaryOperator(And, e1, e2)) }
| e1=expression OR e2=expression    { make_expr (BinaryOperator(Or, e1, e2)) }
| e1=expression BAND e2=expression  { make_expr (BinaryOperator(BAnd, e1, e2)) }
| e1=expression BOR e2=expression   { make_expr (BinaryOperator(BOr, e1, e2)) }
| e1=expression BXOR e2=expression  { make_expr (BinaryOperator(BXor, e1, e2)) }
| e1=expression LSL e2=expression   { make_expr (BinaryOperator(Lsl, e1, e2)) }
| e1=expression ASR e2=expression   { make_expr (BinaryOperator(Asr, e1, e2)) }
| id=IDENT                          { make_expr (Get(id)) }
| s=subscript                       { let id, i = s in make_expr (Read (make_expr (Get id), i)) }
| f=IDENT LPAR a=separated_list(COMMA, expression) RPAR 
                                    { make_expr (Call(f, a)) }
;
