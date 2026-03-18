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
  | InA of int
  | InP of block * string list * env
  | InPR of block * string * string list * env
  | None

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
    (k, v) ::env
let rec env_updates keys values env =
  match (keys, values) with
    | ([], []) -> env
    | (k::ks, v::vs) -> env_updates ks vs (env_update k v env)
    | _ -> failwith "Error : env_updates" 

(*MEMOIRE : unchecked InA -> InZ *)
type memoire = (int * value) list 
let mem_init = []
let find_var_mem x mem = 
  match List.assoc_opt x mem with
    | Some v -> v
    | None -> failwith ("Error : variable not found")

let count = ref 1
let new_address () = let a = !count in count := !count + 1; a
let allocation (mem : memoire) : (int * memoire) = let newA = new_address () in (newA, (newA, None) :: mem )
let modification (mem : memoire) (a : int) (v : value) : memoire = 
  if List.exists (fun (ad, _) -> ad = a) mem then
    List.map (fun (ad, value) -> if ad = a then (ad, v) else (ad, value)) mem
  else
    failwith ("Error : adress not found")

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


(*ARG*)
let rec i_arg a =
  match a with
    | ASTArg (s, _) -> s
and i_args al = List.map i_arg al

(*EXPR*)
let rec i_expr rho sigma e =
  match e with
    | ASTNum n -> InZ(n)
    | ASTId ("true") -> InZ(1)
    | ASTId ("false") -> InZ(0)
    | ASTId x -> (match find_var x rho with
                    | InA a -> find_var_mem a sigma
                    | v -> v)
    | ASTIf(econd, econs, ealt) -> if i_expr rho sigma econd = InZ(1) then i_expr rho sigma econs else i_expr rho sigma ealt
    | ASTAnd(e1, e2) -> if i_expr rho sigma e1 = InZ(1) then i_expr rho sigma e2 else InZ(0)
    | ASTOr(e1, e2) -> if i_expr rho sigma e1 = InZ(1) then InZ(1) else i_expr rho sigma e2 
    | ASTApp(ASTId("not"), [e]) ->  (match i_expr rho sigma e with 
                                      | InZ n -> InZ(pi1 "not" n) 
                                      | _ -> failwith "Error : prim not")
    | ASTApp(ASTId s, [e1; e2]) when s = "eq" || s = "lt" || s = "add" || s = "sub" || s = "mul" || s = "div" ->
        (match i_expr rho sigma e1, i_expr rho sigma e2 with 
          |InZ n1, InZ n2 -> InZ(pi2 s n1 n2) 
          | _ -> failwith "Error : binary prim")
    | ASTApp(e, es) ->  (match i_expr rho sigma e with 
                          | InF(fe, fal, frho) -> let v = (i_exprs rho sigma es) in 
                                                  let rho2 = env_updates fal v frho in
                                                  i_expr rho2 sigma fe
                          | InFR(fe, fs, fal, frho) ->  let v = (i_exprs rho sigma es) in 
                                                        let rho2 = env_update fs (InFR(fe, fs, fal, frho)) (env_updates fal v frho) in
                                                        i_expr rho2 sigma fe
                          | _ -> failwith "Error : expression is not a function")
    | ASTAbs(al, e) -> InF(e, i_args al, rho)
and i_exprs rho sigma es = List.map (i_expr rho sigma) es

(*DEF*)
let i_def rho sigma d = 
  match d with 
    | ASTConst(s, _, e) -> let v = i_expr rho sigma e in (env_update s v rho, sigma)
    | ASTFun(s, _, al, e) -> let v = InF(e, i_args al, rho) in (env_update s v rho, sigma)
    | ASTFunRec(s, _, al, e) -> let v = InFR(e, s, i_args al, rho) in (env_update s v rho, sigma)
    | ASTVar(s, _) -> let (a, sigma2) = allocation sigma in (env_update s (InA a) rho, sigma2)
    | ASTProc(s, al, b) -> let v = InP(b, i_args al, rho) in (env_update s v rho, sigma)
    | ASTProcRec(s, al, b) -> let v = InPR(b, s, i_args al, rho) in (env_update s v rho, sigma)

(*STAT*)
let rec i_stat rho sigma omg s =
  match s with
    | ASTEcho e -> (match i_expr rho sigma e with 
                    | InZ n -> (sigma, Flux (n, omg)) 
                    | _ -> failwith "Error : echo")
    | ASTSet(s, e) -> (match find_var s rho with 
                      | InA a -> let v = i_expr rho sigma e in (modification sigma a v, omg) 
                      | _ -> failwith "Error : set")
    | ASTIf2(e, b1, b2) -> if i_expr rho sigma e = InZ(1) then i_block rho sigma omg b1 else i_block rho sigma omg b2
    | ASTWhile(e, b) -> if i_expr rho sigma e = InZ(1) 
                            then (let (sigma2, omg2) = i_block rho sigma omg b in i_stat rho sigma2 omg2 s)
                            else (sigma, omg)
    | ASTCall(s, es) -> (match find_var s rho with 
                        | InP(b, al, rho2) -> let v = (i_exprs rho sigma es) 
                                              in let rho3 = env_updates al v rho2 
                                              in i_block rho3 sigma omg b
                        | InPR(b, s, al, rho2) -> let v = (i_exprs rho sigma es) 
                                                  in let rho3 = env_update s (InPR(b, s, al, rho2)) (env_updates al v rho2) 
                                                  in i_block rho3 sigma omg b
                        | _ -> failwith "Error : expression is not a procedure")

(*CMDS*)
and i_cmd rho sigma omg c =
  match c with
    | ASTStat s -> i_stat rho sigma omg s
    | ASTDef(d, co) -> let (rho2, sigma2) = i_def rho sigma d in i_cmd rho2 sigma2 omg co
    | ASTStats(s, co) -> let (sigma2, omg2) = i_stat rho sigma omg s in i_cmd rho sigma2 omg2 co

(*BLOCK*)
and i_block rho sigma omg b = 
  match b with
    | ASTCmds c -> i_cmd rho sigma omg c

(*PROG*)
let i_prog p = i_block env_init mem_init Eps p




