%{

  open Lexing
  open Minic_ast

  (* Valeur par défaut d'une variable d'un type donné *)
  let default_value = function
   | Int -> Cst 0
   | Bool -> BCst false
   | Void -> Cst 0      (* peu importe, sera détécté par le vérificateur de type *)

%}

(* Déclaration des lexèmes *)
%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT
%token LPAR RPAR BEGIN END
%token RETURN SET SEMI COMMA
%token IF ELSE WHILE PUTCHAR
%token INT BOOL VOID
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
| dl=declaration_list EOF
       { let var_list, fun_list = dl in
         { globals = var_list; functions = fun_list; } }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

(* Chaque déclaration peut concerner une variable ou une fonction. *)
declaration_list:
| (* vide *) { [], [] }    
| vd=variable_decl dl=declaration_list { let vl, fl = dl in
                                         vd :: vl, fl }
| fd=function_decl dl=declaration_list { let vl, fl = dl in
                                         vl, fd :: fl }
;

(* Déclaration de variable. *)
variable_decl:
| t=typ x=IDENT SET e=expression SEMI { (x, t, e) }
| t=typ x=IDENT SEMI { (x, t, default_value t) }
;

(* Indication de type. *)
typ:
| INT   { Int }
| BOOL  { Bool }
| VOID  { Void }
;

(* Déclaration de fonction. *)
function_decl:
| t=typ f=IDENT LPAR p=separated_list(COMMA, parameter) RPAR BEGIN l=list(variable_decl) s=list(instruction) END
    { { name=f; code=s; params=p; return=t; locals=l } }
;

(* Paramètre formel d'une fonction *)
parameter:
| t=typ id=IDENT { id, t }
;

(* Instructions. *)
instruction:
| PUTCHAR LPAR e=expression RPAR SEMI  { Putchar(e) }
| id=IDENT SET e=expression SEMI       { Set(id, e) }
| IF LPAR c=expression RPAR BEGIN t=list(instruction) END ELSE BEGIN f=list(instruction) END
                                       { If(c, t, f) }
| WHILE LPAR c=expression RPAR BEGIN s=list(instruction) END
                                       { While(c, s) }
| RETURN e=expression SEMI             { Return(e) }
| e=expression SEMI                    { Expr(e) }
;

(* Expressions. *)
expression:
| LPAR e=expression RPAR           { e }
| n=CST                            { Cst(n) }
| b=BOOL_CST                       { BCst(b) }
| SUB e=expression                 { UnaryOperator(Minus, e) }
| ADD e=expression                 { e }
| NOT e=expression                 { UnaryOperator(Not, e) }
| BNOT e=expression                { UnaryOperator(BNot, e) }
| e1=expression ADD e2=expression  { BinaryOperator(Add, e1, e2) }
| e1=expression SUB e2=expression  { BinaryOperator(Sub, e1, e2) }
| e1=expression MUL e2=expression  { BinaryOperator(Mult, e1, e2) }
| e1=expression DIV e2=expression  { BinaryOperator(Div, e1, e2) }
| e1=expression MOD e2=expression  { BinaryOperator(Mod, e1, e2) }
| e1=expression EQ e2=expression   { BinaryOperator(Eq, e1, e2) }
| e1=expression NEQ e2=expression  { BinaryOperator(Neq, e1, e2) }
| e1=expression LT e2=expression   { BinaryOperator(Lt, e1, e2) }
| e1=expression LEQ e2=expression  { BinaryOperator(Leq, e1, e2) }
| e1=expression GT e2=expression   { BinaryOperator(Gt, e1, e2) }
| e1=expression GEQ e2=expression  { BinaryOperator(Geq, e1, e2) }
| e1=expression AND e2=expression  { BinaryOperator(And, e1, e2) }
| e1=expression OR e2=expression   { BinaryOperator(Or, e1, e2) }
| e1=expression BAND e2=expression { BinaryOperator(BAnd, e1, e2) }
| e1=expression BOR e2=expression  { BinaryOperator(BOr, e1, e2) }
| e1=expression BXOR e2=expression { BinaryOperator(BXor, e1, e2) }
| e1=expression LSL e2=expression  { BinaryOperator(Lsl, e1, e2) }
| e1=expression ASR e2=expression  { BinaryOperator(Asr, e1, e2) }
| id=IDENT                         { Get(id) }
| f=IDENT LPAR a=separated_list(COMMA, expression) RPAR { Call(f, a) }
;
