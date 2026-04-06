(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
open Format

let sep_cma fmt () = fprintf fmt ", "

let pp_lst_cma p = pp_print_list ~pp_sep:sep_cma p

(*TYPE*)
let rec pp_stype fmt t =
  match t with
    | ASTInt -> fprintf fmt "int"
    | ASTBool -> fprintf fmt "bool"
    | ASTVec t -> fprintf fmt "vec(%a)" pp_stype t
let rec pp_type fmt t =
  match t with
    | ASTSType st -> pp_stype fmt st
    | ASTTypeT(ts, t) -> fprintf fmt "arrow([%a],%a)" pp_types ts pp_type t
and pp_types fmt ts = pp_lst_cma pp_type fmt ts

(*ARG*)
let rec pp_arg fmt a =
  match a with
    | ASTArg (s, t) -> fprintf fmt "(%s,%a)" s pp_type t
and pp_args fmt al = pp_lst_cma pp_arg fmt al

(*ARGP*)
let rec pp_argp fmt a =
  match a with
    | ASTArgp (s, t) -> fprintf fmt "(%s,%a)" s pp_type t
    | ASTArgVar (s, t) -> fprintf fmt "(var2(%s),%a)" s pp_type t
and pp_argsp fmt al = pp_lst_cma pp_argp fmt al

(*EXPR*)
let rec pp_expr fmt e =
  match e with
    | ASTNum n -> fprintf fmt "num(%d)" n
    | ASTId x -> fprintf fmt "ident(%s)" x
    | ASTIf(econd, econs, ealt) -> fprintf fmt "if(%a,%a,%a)" pp_expr econd pp_expr econs pp_expr ealt
    | ASTAnd(e1, e2) -> fprintf fmt "and(%a,%a)" pp_expr e1 pp_expr e2
    | ASTOr(e1, e2) -> fprintf fmt "or(%a,%a)" pp_expr e1 pp_expr e2
    | ASTApp(e, es) -> fprintf fmt "app(%a,[%a])" pp_expr e  pp_exprs es
    | ASTAbs(al, e) -> fprintf fmt "abs([%a],%a)" pp_args al  pp_expr e
    | ASTAlloc e -> fprintf fmt "alloc(%a)" pp_expr e
    | ASTLen e -> fprintf fmt "len(%a)" pp_expr e
    | ASTNth(e1, e2) -> fprintf fmt "nth(%a,%a)" pp_expr e1 pp_expr e2
    | ASTVset(e1, e2, e3) -> fprintf fmt "vset(%a,%a,%a)" pp_expr e1 pp_expr e2 pp_expr e3
and pp_exprs fmt es = pp_lst_cma pp_expr fmt es

(*EXPRP*)
let rec pp_exprp fmt e =
  match e with
    | ASTVal ex -> fprintf fmt "%a" pp_expr ex
    | ASTRef s -> fprintf fmt "adr(%s)" s
and pp_exprsp fmt es = pp_lst_cma pp_exprp fmt es

(*LVAL*)
let rec pp_lval fmt l =
  match l with
    | ASTId s -> fprintf fmt "ident(%s)" s
    | ASTNth(l, e) -> fprintf fmt "nth(%a,%a)" pp_lval l pp_expr e

(*STAT*)
let rec pp_stat fmt s = 
  match s with
    | ASTEcho e -> fprintf fmt "echo(%a)" pp_expr e
    | ASTSet(l, e) -> fprintf fmt "set(%a,%a)" pp_lval l pp_expr e
    | ASTIf2(e, b1, b2) -> fprintf fmt "if2(%a,%a,%a)" pp_expr e pp_block b1 pp_block b2
    | ASTWhile(e, b) -> fprintf fmt "while(%a,%a)" pp_expr e pp_block b
    | ASTCall(s, es) -> fprintf fmt "call(%s,[%a])" s pp_exprsp es

(*DEF*)
and pp_def fmt d = 
  match d with 
    | ASTConst(s, t, e)-> fprintf fmt "const(%s,%a,%a)" s pp_type t pp_expr e
    | ASTFun(s, t, al, e) -> fprintf fmt "fun(%s,%a,[%a],%a)" s pp_type t pp_args al pp_expr e
    | ASTFunRec(s, t, al, e) -> fprintf fmt "funrec(%s,%a,[%a],%a)" s pp_type t pp_args al pp_expr e
    | ASTVar(s, t)-> fprintf fmt "var(%s,%a)" s pp_stype t
    | ASTProc(s, al, b) -> fprintf fmt "proc(%s,[%a],%a)" s pp_argsp al pp_block b
    | ASTProcRec(s, al, b) -> fprintf fmt "procrec(%s,[%a],%a)" s pp_argsp al pp_block b

(*CMDS*)
and pp_cmd fmt c =
  match c with
    | ASTStat s -> fprintf fmt "stat(%a)" pp_stat s
    | ASTDef(d, co) -> fprintf fmt "def(%a,%a)" pp_def d pp_cmd co
    | ASTStats(s, co) -> fprintf fmt "stats(%a,%a)" pp_stat s pp_cmd co

(*BLOCK*)
and pp_block fmt b = 
  match b with
    | ASTCmds c -> fprintf fmt "block(%a)" pp_cmd c

let pp_prog fmt p =
  fprintf fmt "prog(%a).\n" pp_block p



