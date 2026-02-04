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

(*EXPR*)
type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTAno of arg list * expr
  | ASTApp of expr * expr list

(*STAT*)
type stat =
    ASTEcho of expr

(*DEF*)
type def =
    ASTConst of string * type1 * expr
  | ASTFun of string * type1 * arg list * expr
  | ASTFunRec of string * type1 * arg list * expr

(*CMDS*)
type cmd =
    ASTStat of stat
  | ASTDef of def * cmd

(*
and cmdsl = cmds list *)

(*PROG
type prog = 
    ASTCmdsL of cmds list
*)