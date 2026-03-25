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
type type1 = 
    ASTInt
  | ASTBool
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

(*EXPRP*)
type exprp = 
    ASTVal of expr
  | ASTRef of string

(*STAT*)
type stat =
    ASTEcho of expr
  | ASTSet of string * expr
  | ASTIf2 of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * exprp list

(*DEF*)
and def =
    ASTConst of string * type1 * expr
  | ASTFun of string * type1 * arg list * expr
  | ASTFunRec of string * type1 * arg list * expr
  | ASTVar of string * type1
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
