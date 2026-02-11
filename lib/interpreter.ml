open Ast

type value = 
    InZ of int
  | InB of bool
  | InF of expr * string list * env
  | InFR of expr * string * string list * env
and type env = [(ASTId(string), value)]
type flux = ??

let env = []
let findvar x = List.assoc_opt x env

let pi1 prim x = 
  match prim with 
    | "not" -> if x = 0 then 1 else 0
let pi2 prim e1 e2 = 
  match prim with 
    | "eq" -> if e1 = e2 then 1 else 0
    | "lt" -> if e1 < e2 then 1 else 0
    | "add" -> e1 + e2
    | "sub" -> e1 - e2
    | "mul" -> e1 * e2
    | "div" -> e1 / e2


let sep_cma  () =   ", "

let i_lst_cma p = ~i_sep:sep_cma p

(*TYPE*)
let rec i_type rho omg t =
  match t with
    | ASTInt ->   "int"
    | ASTBool ->   "bool"
    | ASTTypeT(ts, t) ->   "arrow([%a],%a)" i_types ts i_type t
and i_types  ts = i_lst_cma i_type  ts

(*ARG*)
let rec i_arg rho omg a =
  match a with
    | ASTArg (s, t) ->   "(%s,%a)" s i_type t
and i_args  al = i_lst_cma i_arg  al

(*EXPR. true false abs app appr *)
let rec i_expr rho omg e =
  match e with
    | ASTNum n -> InZ(n)
    | ASTId x -> findvar x rho
    | ASTIf(econd, econs, ealt) -> if i_expr rho omg econd = InZ(1) then i_expr rho omg econs else i_expr rho omg ealt
    | ASTAnd(e1, e2) -> if i_expr rho omg e1 = InZ(1) and i_expr rho omg e2 = InZ(1) then InZ(1) else InZ(0)
    | ASTOr(e1, e2) -> if i_expr rho omg e1 = InZ(1) then InZ(1) else if i_expr rho omg e2 = InZ(1) then InZ(1) else InZ(0)
    | ASTApp(ASTId("not"), [e]) -> match i_expr e with InZ n -> InZ(pi1 "not" n)
    | ASTApp(ASTId s, [e1; e2]) when s = "eq" || s = "lt" || s = "add" || s = "sub" || s = "mul" || s = "div" ->
        let prim = s in match i_expr e1, i_expr e2 with InZ n1, InZ n2 -> InZ(pi2 s n1 n2)
    | ASTApp(e, es) ->   "app(%a,[%a])" i_expr e  i_exprs es
    | ASTAbs(al, e) -> (*match al with 
                        | aux pour avoir que les ident -> inF(e,xl,rho)*)
and i_exprs  es = i_lst_cma i_expr  es

(*STAT*)
let i_stat rho omg s =
  match s with
  ASTEcho e -> match e with InZ n -> n ??


(*DEF*)
let i_def rho omg d = 
  match d with 
    | ASTConst(s, t, e)->   "const(%s,%a,%a)" s i_type t i_expr e
    | ASTFun(s, t, al, e) ->   "fun(%s,%a,[%a],%a)" s i_type t i_args al i_expr e
    | ASTFunRec(s, t, al, e) ->   "funrec(%s,%a,[%a],%a)" s i_type t i_args al i_expr e

(*CMDS*)
let rec i_cmd rho omg  c =
  match c with
    | ASTStat s -> i_stat rho omg s
    | ASTDef(d, co) -> let rho2 = i_def rho omg d in i_cmd rho2 omg co

let i_cmds rho omg cmds =
  match cmds with
    | c :: [] -> i_cmd rho omg c
    | c :: cs -> let rho2 = i_cmd rho omg c in i_cmds rho2 omg cs
    | _ -> failwith "erreur"

let i_prog  p = i_cmds env None p




