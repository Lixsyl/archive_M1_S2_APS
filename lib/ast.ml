(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

(*TYPE*)
type stype =
  | ASTInt
  | ASTBool
  | ASTVec of stype

type type1 = 
    ASTSType of stype
  | ASTTypeT of type1 list * type1

(*ARG*)
type arg =
    ASTArg of string * type1

(*ARGP*)
type argp =
    ASTArgp of string * type1
  | ASTArgVar of string * type1

(*EXPR*)
type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * expr list
  | ASTAbs of arg list * expr
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNth of expr * expr
  | ASTVset of expr * expr * expr

(*EXPRP*)
type exprp = 
    ASTVal of expr
  | ASTRef of string

(*LVAL*)
type lval =
    ASTId of string
  | ASTNth of lval * expr

(*STAT*)
type stat =
    ASTEcho of expr
  | ASTSet of lval * expr
  | ASTIf2 of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * exprp list

(*DEF*)
and def =
    ASTConst of string * type1 * expr
  | ASTFun of string * type1 * arg list * expr
  | ASTFunRec of string * type1 * arg list * expr
  | ASTVar of string * stype
  | ASTProc of string * argp list * block
  | ASTProcRec of string * argp list * block

(*CMDS*)
and cmd =
    ASTStat of stat
  | ASTDef of def * cmd
  | ASTStats of stat * cmd

(*BLOCK*)
and block = 
    ASTCmds of cmd
