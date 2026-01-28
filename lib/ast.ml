(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)


type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTAno of args * expr
  | ASTApp of expr * exprs

type exprs = expr list

type stat =
    ASTEcho of expr

type cmds =
    ASTStat of stat
  | ASTDef of def * cmds

type prog = 
    ASTCmdl of cmds list

type type1 = 
    ASTInt of int
  | ASTBool of bool
  | ASTTypeT of types * type1

type types = type1 list

type arg =
    ASTarg of string * type1

type args = arg list

type def =
    ASTConst of string * type1 * expr
  | ASTFun of string * type1 * args * expr
  | ASTFunRec of string * type1 * args * expr

