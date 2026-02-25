open Ast

(*FLUX*)
type flux = 
    Eps
  | Flux of int * flux
let rec print_flux f =
  match f with
    | Eps -> ""
    | Flux (x, xs) -> string_of_int x ^ print_flux xs

(*VALUE*)
type value = 
    InZ of int
  | InF of expr * string list * env
  | InFR of expr * string * string list * env
  | InA of string
  | InP of cmd * string list * env
  | InPR of cmd * string * string list * env

(*ENV*)
and env = (string * value) list

let env_init = []
let find_var x env = 
  match List.assoc_opt x env with
    | Some v -> v
    | None -> failwith ("Error : variable not found")
let env_update k v env =
  if List.exists (fun (key, _) -> key = k) env then
    List.map (fun (key, value) -> if key = k then (key, v) else (key, value)) env
  else
    env @ [(k, v)]
let rec env_updates keys values env =
  match (keys, values) with
    | ([], []) -> env
    | (k::ks, v::vs) -> env_updates ks vs (env_update k v env)
    | _ -> failwith "Error : env_updates" 

(*MEMOIRE : unchecked InA -> InZ *)
type memoire = (value * value) list 

(*PRIMITIVES*)
let pi1 prim x = 
  match prim with 
    | "not" -> if x = 0 then 1 else 0
    | _ -> failwith "Error : pi1"
let pi2 prim e1 e2 = 
  match prim with 
    | "eq" -> if e1 = e2 then 1 else 0
    | "lt" -> if e1 < e2 then 1 else 0
    | "add" -> e1 + e2
    | "sub" -> e1 - e2
    | "mul" -> e1 * e2
    | "div" -> e1 / e2
    | _ -> failwith "Error : pi2"

(*TYPE
let rec i_type rho omg t =
  match t with
    | ASTInt ->   "int"
    | ASTBool ->   "bool"
    | ASTTypeT(ts, t) ->   "arrow([%a],%a)" i_types ts i_type t
and i_types  ts = i_lst_cma i_type  ts
*)

(*ARG*)
let rec i_arg a =
  match a with
    | ASTArg (s, _) -> s
and i_args al = List.map i_arg al

(*EXPR*)
let rec i_expr rho omg e =
  match e with
    | ASTNum n -> InZ(n)
    | ASTId ("true") -> InZ(1)
    | ASTId ("false") -> InZ(0)
    | ASTId x -> find_var x rho
    | ASTIf(econd, econs, ealt) -> if i_expr rho omg econd = InZ(1) then i_expr rho omg econs else i_expr rho omg ealt
    | ASTAnd(e1, e2) -> if i_expr rho omg e1 = InZ(1) then i_expr rho omg e2 else InZ(0)
    | ASTOr(e1, e2) -> if i_expr rho omg e1 = InZ(1) then InZ(1) else i_expr rho omg e2 
    | ASTApp(ASTId("not"), [e]) ->  begin match i_expr rho omg e with 
                                      | InZ n -> InZ(pi1 "not" n) 
                                      | _ -> failwith "Error : prim not"
                                    end
    | ASTApp(ASTId s, [e1; e2]) when s = "eq" || s = "lt" || s = "add" || s = "sub" || s = "mul" || s = "div" ->
        begin match i_expr rho omg e1, i_expr rho omg e2 with 
          |InZ n1, InZ n2 -> InZ(pi2 s n1 n2) 
          | _ -> failwith "Error : binary prim"
        end
    | ASTApp(e, es) ->  begin match i_expr rho omg e with 
                          | InF(fe, fal, frho) -> let v = (i_exprs rho omg es) in 
                                                  let rho2 = env_updates fal v frho in
                                                  i_expr rho2 omg fe
                          | InFR(fe, fs, fal, frho) ->  let v = (i_exprs rho omg es) in 
                                                        let rho2 = env_update fs (InFR(fe, fs, fal, frho)) (env_updates fal v frho) in
                                                        i_expr rho2 omg fe
                          | _ -> failwith "Error : expression is not a function"
                        end
    | ASTAbs(al, e) -> InF(e, i_args al, rho)
and i_exprs rho omg es = List.map (i_expr rho omg) es

(*STAT*)
let i_stat rho omg s =
  match s with
    | ASTEcho e -> match i_expr rho omg e with InZ n -> Flux (n, omg) | _ -> failwith "Error : stat"
(*    | ASTSet(s, e) -> fprintf fmt "set(%s,%a)" s pp_expr e
    | ASTIf2(e, b1, b2) -> fprintf fmt "if2(%a,%a,%a)" pp_expr e pp_block b1 pp_block b2
    | ASTWhile(e, b) -> fprintf fmt "while(%a,%a)" pp_expr e pp_block b
    | ASTCall(s, es) -> fprintf fmt "call(%s,%a)" s pp_exprs es 
*)

(*DEF*)
let i_def rho omg d = 
  match d with 
    | ASTConst(s, _, e) -> let v = i_expr rho omg e in env_update s v rho 
    | ASTFun(s, _, al, e) -> let v = InF(e, i_args al, rho) in env_update s v rho 
    | ASTFunRec(s, _, al, e) -> let v = InFR(e, s, i_args al, rho) in env_update s v rho 

(*CMDS*)
let rec i_cmd rho omg c =
  match c with
    | ASTStat s -> i_stat rho omg s
    | ASTDef(d, co) -> let rho2 = i_def rho omg d in i_cmd rho2 omg co

let i_cmds rho omg cmds =
  match cmds with
    | c :: [] -> i_cmd rho omg c
    | _ -> failwith "erreur"
(*PROG*)
let i_prog p = i_cmds env_init Eps p




