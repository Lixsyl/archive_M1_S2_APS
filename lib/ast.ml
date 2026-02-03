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
    ASTInt of int
  | ASTBool of bool
  | ASTTypeT of types * type1

and types = type1 list

(*ARG*)
type arg =
    ASTArg of string * type1

type args = arg list

(*EXPR*)
type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTAno of args * expr
  | ASTApp of expr * exprs

and exprs = expr list

(*STAT*)
type stat =
    ASTEcho of expr

(*DEF*)
type def =
    ASTConst of string * type1 * expr
  | ASTFun of string * type1 * args * expr
  | ASTFunRec of string * type1 * args * expr

(*CMDS*)
type cmds =
    ASTStat of stat
  | ASTDef of def * cmds

(*
and cmdsl = cmds list *)

(*PROG
type prog = 
    ASTCmdsL of cmds list
*)