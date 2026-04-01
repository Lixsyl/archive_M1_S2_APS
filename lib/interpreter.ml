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
  | InB of value * int
  | None

(*ENV*)
and env = (string * value) list

let env_init = []
let find_var x env = 
  match List.assoc_opt x env with
    | Some v -> v
    | None -> failwith ("Error : variable not found")
let env_update (k : string) (v : value) (env : env) =
  if List.exists (fun (key, _) -> key = k) env then
    List.map (fun (key, value) -> if key = k then (key, v) else (key, value)) env
  else
    (k, v) ::env
let rec env_updates keys values env =
  match (keys, values) with
    | ([], []) -> env
    | (k::ks, v::vs) -> env_updates ks vs (env_update k v env)
    | _ -> failwith "Error : env_updates" 

(*MEMOIRE : unchecked InA -> InZ/InB *)
type memoire = (value * value) list 
let mem_init = []
let find_var_mem x (mem : memoire) = 
  let x_val = InA x in
  match List.assoc_opt x_val mem with
    | Some v -> v
    | None -> failwith ("Error : variable not found")

let count = ref 1
let new_address () = let a = !count in count := !count + 1; a
let allocation (mem : memoire) : (value * memoire) = 
  let newA = InA(new_address()) 
  in (newA, (newA, None) :: mem )
let modification (mem : memoire) (a : int) (v : value) : memoire = 
  let a_val = InA a in
  if List.exists (fun (ad, _) -> ad = a_val) mem then
    List.map(fun (ad, value) -> if ad = a_val then (ad, v) else (ad, value) ) mem
  else
    failwith ("Error : adress not found")

let allocn (mem : memoire) n : (value * memoire) = 
  let (addr,_) = allocation mem in
  let rec aux mem i =
    if i <= 0 then mem
    else let (_, mem2) = allocation mem in aux mem2 (i - 1)
  in (addr, aux mem (n-1))

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

(*ARGP*)
let rec i_argp a =
  match a with
    | ASTArgp (s, _) -> s
    | ASTArgVar (s, _) -> s
and i_argsp al = List.map i_argp al

(*EXPR*)
let rec i_expr rho sigma e =
  match e with
    | ASTNum n -> (InZ(n), sigma)
    | ASTId ("true") -> (InZ(1), sigma)
    | ASTId ("false") -> (InZ(0), sigma)
    | ASTId x -> (match find_var x rho with
                    | InA a -> (find_var_mem a sigma, sigma)
                    | v -> (v, sigma))
    | ASTIf(econd, econs, ealt) ->  let (cond,sigma2) = i_expr rho sigma econd in
                                    let (cons,sigma3) = i_expr rho sigma2 econs in
                                    if cond = InZ(1) then (cons, sigma3) else i_expr rho sigma3 ealt 
    | ASTAnd(e1, e2) -> let (ex1,sigma2) = i_expr rho sigma e1 in
                        if ex1 = InZ(0) then (InZ(0), sigma2) else i_expr rho sigma2 e2
    | ASTOr(e1, e2) ->  let (ex1,sigma2) = i_expr rho sigma e1 in
                        if ex1 = InZ(1) then (InZ(1), sigma2) else i_expr rho sigma2 e2
    | ASTApp(ASTId("not"), [e]) ->  (match i_expr rho sigma e with 
                                      | (InZ n, sigma2) -> (InZ(pi1 "not" n), sigma2)
                                      | _ -> failwith "Error : prim not")
    | ASTApp(ASTId s, [e1; e2]) when s = "eq" || s = "lt" || s = "add" || s = "sub" || s = "mul" || s = "div" ->
        let (n1, sigma2) = i_expr rho sigma e1 in
        let (n2, sigma3) = i_expr rho sigma2 e2 in
        (match n1, n2 with 
            | (InZ n1, InZ n2) -> (InZ(pi2 s n1 n2), sigma3)
            | _ -> failwith "Error : binary prim")
    | ASTApp(e, es) ->  (match i_expr rho sigma e with 
                          | (InF(fe, fal, frho), _) ->  let v = (i_exprs rho sigma es) in 
                                                        let rho2 = env_updates fal v frho in
                                                        i_expr rho2 sigma fe
                          | (InFR(fe, fs, fal, frho), _) -> let v = (i_exprs rho sigma es) in 
                                                            let rho2 = env_update fs (InFR(fe, fs, fal, frho)) (env_updates fal v frho) in
                                                            i_expr rho2 sigma fe
                          | _ -> failwith "Error : expression is not a function")
    | ASTAbs(al, e) -> (InF(e, i_args al, rho), sigma)
    | ASTAlloc e -> (match i_expr rho sigma e with 
                      | (InZ n, sigma2)-> let (a, sigma3) = allocn sigma2 n in (InB(a,n), sigma3)
                      | _ -> failwith "Error : alloc")
    | ASTLen e -> (match i_expr rho sigma e with 
                    | (InB(_, n), sigma2) -> (InZ n, sigma2)
                    | _ -> failwith "Error : len")
    | ASTNth(e1, e2) -> (match i_expr rho sigma e1 with 
                          | (InB(InA a, _), sigma2) -> (match i_expr rho sigma2 e2 with 
                                                      | (InZ i, sigma3) -> (find_var_mem (a+i) sigma3, sigma3)
                                                      | _ -> failwith "Error : nth : e2 not inZ")
                          | _ -> failwith "Error : nth : e1 not inB")
    | ASTVset(e1, e2, e3) -> (match i_expr rho sigma e1 with 
                              | (InB(InA a, n), sigma2) -> 
                                    (match i_expr rho sigma2 e2 with 
                                    | (InZ i, sigma3) -> 
                                          (match i_expr rho sigma3 e3 with 
                                          | (v, sigma4) -> let sigma5 = modification sigma4 (a+i) v 
                                                            in (InB(InA a, n), sigma5))
                                    | _ -> failwith "Error : vset : e2 not inZ")
                              | _ -> failwith "Error : vset : e1 not inB")
and i_exprs rho sigma es = List.map (fun e -> fst (i_expr rho sigma e)) es

(*EXPRP*)
let rec i_exprp rho sigma e =
  match e with
    | ASTVal ex -> i_expr rho sigma ex
    | ASTRef s -> (find_var s rho, sigma)
and i_exprsp rho sigma es = List.map (fun e -> fst (i_exprp rho sigma e)) es

(*LVAL*)
let rec i_lval rho sigma l =
  match l with
    | ASTId s -> (match find_var s rho with
                  | InA a -> (a, sigma)
                  | _ -> failwith "Error : lval : variable is not an address")
    | ASTNth(ASTId l, e) -> (match find_var l rho with 
                            | InB(InA a, _) -> (match i_expr rho sigma e with 
                                                | (InZ i, sigma2) -> (a+i, sigma2)
                                                | _ -> failwith "Error : lval : e not inZ")
                            | _ -> failwith "Error : lval : l not inB")
    | ASTNth(l, e) -> let (a, sigma2) = i_lval rho sigma l in
                      (match find_var_mem a sigma2 with 
                        | InB(InA a2, _) -> (match i_expr rho sigma2 e with 
                                        | (InZ i, sigma3) -> (a2+i, sigma3)
                                        | _ -> failwith "Error : lval : e not inZ")
                        | _ -> failwith "Error : lval : variable is not an array")

(*DEF*)
let i_def rho sigma d = 
  match d with 
    | ASTConst(s, _, e) -> let (v, sigma2) = i_expr rho sigma e in (env_update s v rho, sigma2)
    | ASTFun(s, _, al, e) -> let v = InF(e, i_args al, rho) in (env_update s v rho, sigma)
    | ASTFunRec(s, _, al, e) -> let v = InFR(e, s, i_args al, rho) in (env_update s v rho, sigma)
    | ASTVar(s, _) -> let (a, sigma2) = allocation sigma in (env_update s a rho, sigma2)
    | ASTProc(s, al, b) -> let v = InP(b, i_argsp al, rho) in (env_update s v rho, sigma)
    | ASTProcRec(s, al, b) -> let v = InPR(b, s, i_argsp al, rho) in (env_update s v rho, sigma) 

(*STAT*)
let rec i_stat rho sigma omg s =
  match s with
    | ASTEcho e -> (match i_expr rho sigma e with 
                    | (InZ n, sigma2) -> (sigma2, Flux (n, omg)) 
                    | _ -> failwith "Error : echo")
    | ASTSet(l, e) -> let (a, _) = i_lval rho sigma l in
                      let (v, sigma2) = i_expr rho sigma e in
                      (modification sigma2 a v, omg) 
    | ASTIf2(e, b1, b2) ->  let (cond, sigma2) = i_expr rho sigma e
                            in if cond = InZ(1) then i_block rho sigma2 omg b1 else i_block rho sigma2 omg b2
    | ASTWhile(e, b) -> let (cond, sigma2) = i_expr rho sigma e 
                        in if cond = InZ(0) then (sigma2, omg)
                        else (let (sigma3, omg2) = i_block rho sigma2 omg b in i_stat rho sigma3 omg2 s)
    | ASTCall(s, es) -> (match find_var s rho with 
                        | InP(b, al, rho2) -> let v = (i_exprsp rho sigma es) 
                                              in let rho3 = env_updates al v rho2 
                                              in i_block rho3 sigma omg b
                        | InPR(b, s, al, rho2) -> let v = (i_exprsp rho sigma es) 
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




