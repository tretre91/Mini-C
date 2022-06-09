# Structure du code

## 0. Préprocesseur (`preprocessor.mll`)

#### Entrée :

Fichier source minic

#### Sortie :

Fichier minic sans directives de préprocesseur

#### Description :

Phase de preprocessing, le préprocesseur ne gère pour l'instant que les déclarations `#include`, il s'occupe de chercher le fichier inclus et copier son contenu dans le fichier sortie.

## 1. Vérification de types (`minic_typechecker.ml`)

Types utilisés : voir [minic_ast.ml](./lib/minic_ast.ml) ou [en annexe](#AST-typé)

#### Entrée :

Arbre de syntaxe abstraite issu de l'analyse syntaxique

#### Sortie :

Le même arbre de syntaxe abstraite, où chaque expression possède un type et une indication sur sa calculabilité à la compilation

#### Opérations :

- Déduit le type de chaque expression du programme
- Vérifie la cohérence des types
- Indique si une expression est "constante" (définition c d'une expression constante)
- Ajoute des conversions de types implicites

#### Description :

Phase de vérification des types, et 1ère phase de la compilation, à chaque expression est associé un type et un booléen `const`, une expression constante est une expression uniquement composée de littéraux et d'opérateurs.


## 2. AST typé -> AST (`minic.ml`)

Types utilisés : voir [minic.ml](./lib/minic.ml) ou [en annexe](#AST-non-typé)

#### Entrée :

Arbre de syntaxe abstraite typé

#### Sortie :

Arbre de syntaxe abstraite non typé

#### Opérations :

- Suppression des types associés aux expressions
- Calcul des expressions constantes
- Remplacement des noms de variables locales par des identifiants uniques (à l'échelle de la fonction)
- Transformation des tableaux en pointeurs
- Ajout d'une fonction `__init` permettant d'initialiser les variables globales
- Récupération des données qui devront être placées en mémoire

#### Description :

À la fin de cette étape, les noms de variables locales ont été traduits en des identifiants uniques à l'échelle d'une fonction. On a également connaissance de toutes les données qui devront être placées en mémoire statique, et donc de la taille de la "section data" du programme. Les expressions constantes (ex : taille des tableaux) sont également calculées à cette étape.

On ajoute également une fonction spéciale qui a pour rôle d'initialiser les variables globales, en WebAssembly les variables globales ne peuvent être initialisées qu'avec des constantes ([pour l'instant](https://github.com/WebAssembly/extended-const/blob/master/proposals/extended-const/Overview.md)), d'où la nécessité d'ajouter une fonction permettant d'initialiser ces variables avec des expressions impliquant des opérateurs

```
;; valide
(global $v (mut i32) i32.const 3)
;; invalide
(global $v (mut i32)
	i32.const 3
	i32.const 2
	i32.add
)
```

(remarque : on pourrait également calculer la valeur finale de ces expressions à la compilation, et utiliser le résultat pour initialiser directement la variable globale en question)


## 3. AST -> LLIR (`minic_llir.ml`)

Types utilisés : voir [llir.ml](./lib/llir.ml) ou [en annexe](#LLIR)

#### Entrée :

Arbre de syntaxe abstraite non typé

#### Sortie :

Représentation intermédiaire sous forme de pile

#### Description :

Cette phase traduit le programme sous forme d'AST en un programme équivalent mais ayant une représentation sous forme de pile, plus proche du programme wasm final.


## 4. LLIR -> Wasm (`minic_wasm.ml`)

Types utilisés : voir [wasm.ml](./lib/wasm.ml) ou [en annexe](#Wasm)

#### Entrée :

Programme sous représentation intermédiaire LLIR

#### Sortie :

Programme WebAssembly

#### Opérations :

- Traduction directe des fonctions LLIR
- Ajout des nœuds (`start`, `memory`, ...) nécessaires à exécution

#### Description :

Cette phase traduit le programme sous représentation LLIR en programme wasm. Un programme WebAssembly est constitué d'un nœud `module` suivi du contenu de ce module, lors de cette phase on ajoute les nœuds suivantes au module :

- `(start $__init)` : permet d'appeler la fonction `__init` lorsque le module est instancié
- `(memory n)` : ajoute une zone de mémoire au module, avec une taille initiale de `n` pages (n\*64KiB)
- `(data ...)` : ajoute des données à la section `data` du module (pour l'instant sert à stocker les initialisateurs des tableaux)
- `(global ...)` pour chaque variable globale
- `(func ...)` pour chaque fonction du programme


## 4bis. Affichage du programme (`minic_wasm.ml`)

#### Entrée :

Programme sous représentation wasm (type `expr`)

#### Sortie :

Affichage du code .wat (WebAssembly text) correspondant


# Annexe

## Types utilisés par les différentes représentations

#### AST typé

```ocaml
(** Représentation des types. *)
type integral_type =
  | Char
  | Int

type constant =
  | CInteger of integral_type * Int64.t
  | CBool of bool
  | CIList of constant list

(** Types des opérations binaires *)
type binop =
  | Add | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Leq | Gt | Geq
  | And | Or
  | BAnd | BOr | BXor
  | Lsl | Asr

(** Types des opérations unaires *)
type unop =
  | Minus
  | Not
  | BNot

type typ =
  | Integer of integral_type
  | Bool
  | Void
  | Ptr of typ
  | Tab of typ * expr
(** Représentation des expressions. *)
and expr_s =
  | Cst of constant
  | Cast of expr * typ * typ
  | InitList of expr list
  | UnaryOperator of unop * expr
  | BinaryOperator of binop * expr * expr
  | Get of string
  | Read of expr * expr (* adresse * offset *)
  | Call of string * expr list
(** Représentation d'une expression typée *)
and expr = {
  t: typ;
  const: bool;
  expr: expr_s
}

(** Type d'une déclaration de variable *)
type var_decl = string * typ * expr

(** Représentation des instructions. *)
type instr =
  | Decl of var_decl list
  | Set of string * expr
  | Write of expr * expr * expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
  | Expr of expr
  | Block of block
(** Type des blocs de code *)
and block = instr list

(** Représentation des fonctions. *)
type fun_def = {
  name: string;
  params: (string * typ) list;
  return: typ;
  body: block;
}

(** Type d'un déclaration dans la portée globale (une déclaration de variable
    globale ou de fonction) *)
type global_decl =
  | Variable of var_decl
  | Function of fun_def
  | ForwardDecl of fun_def

(** Représentation des programmes.
    Un programme est simplement une suite de déclarations de variables ou de fonctions. *)
type prog = global_decl list
```


#### AST non typé

```ocaml
type typ =
  | Integer of integral_type
  | Bool
  | Void
  | Ptr of typ
  | Tab of typ * int

(** Représentation des expressions, on retire les informations de type de l'AST *)
type expr =
  | Cst of const_expr
  | Cast of expr * typ * typ
  | InitList of bool * expr list
  | UnaryOperator of typ * unop * expr
  | BinaryOperator of typ * binop * expr * expr
  | Get of string
  | Read of typ * expr
  | Call of string * expr list
(** Type des valeurs connues à la compilation *)
and const_value =
  | Integral of Int64.t
  | Floating of float
  | List of const_expr list
and const_expr = {
  t: typ;
  value: const_value;
}

(** Représentation des instructions, on n'utilise plus le constructeur de déclaration
    de variable dans cette représentation *)
type instr =
  | Set of string * expr
  | StaticMemcpy of expr * int * int (* dest, id du segment, taille du segment *)
  | Write of typ * expr * expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
  | Expr of expr
  | Block of block
(** Type des blocs de code *)
and block = instr list

type fun_def = {
  name: string;
  params: (string * typ) list;
  locals: (string * typ) list;
  return: typ;
  body: block;
}

type prog = {
  static: (int * string) list; (* Données mémoire *)
  persistent: string list; (* Données persistentes, par ex : initialisateur des tableaux*)
  globals: (string * typ) list;
  functions: fun_def list;
  extern_functions: fun_def list;
}
```


#### LLIR

```ocaml
type var =
  | Global of string
  | Param of int
  | Local of int

type num_op =
  | Add | Sub | Mul | Div | Mod
  | Eq | Eqz | Neq | Lt | Leq | Gt | Geq
  | And | Or | Not
  | BAnd | BOr | BXor
  | Lsl | Asr

type constant =
  | I32Cst of Int32.t
  | I64Cst of Int64.t
  | F32Cst of float
  | F64Cst of float

type datatype =
  | Int8
  | Int16
  | Int32
  | Int64
  | Float32
  | Float64

type instr =
  | Cst of constant
  | Cast of datatype * datatype
  | Op of Wasm.dtype * num_op
  | Get of var
  | Set of var
  | Load of Wasm.dtype
  | Store of Wasm.dtype
  | MemInit of int
  | If of seq * seq
  | While of seq * seq (* Liste des instructions de la condition d'arrêt + corps de la boucle *)
  | Call of string
  | Return
  | Drop
and seq = instr list

type fun_def = {
  name: string;
  params: Wasm.dtype list;
  locals: Wasm.dtype list;
  return: Wasm.dtype option;
  code: seq;
}

type prog = {
  static: (int option * string) list;
  globals: (string * Wasm.dtype) list;
  functions: fun_def list;
  extern_functions: fun_def list;
}
```


#### Wasm

```ocaml
type dtype = I8 | I16 | I32 | I64 | F32 | F64

type qualifier = Mut | Const

type expr =
  | Instr of string list
  | Module of seq
  | Block of seq
  | Loop of seq
  | If of seq * seq
  | Function of string * dtype list * dtype option * dtype list * seq
  | ImportedFunction of string * dtype list * dtype option
  | Global of string * qualifier * dtype * seq
  | Start of string
  | Memory of int
  | Data of int option * string
  | Comment of string
and seq = expr list
```

