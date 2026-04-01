
main :- read(user_input, X), type_check(X).

type_check(prog(P)) :- type_prog(prog(P)), write("OK\n").
type_check(_) :- write("KO\n").

contexte_initial([
    (true, bool),
    (false, bool),
    (not, arrow([bool], bool)),
    (eq, arrow([int, int], bool)),
    (lt, arrow([int, int], bool)),
    (add, arrow([int, int], int)),
    (sub, arrow([int, int], int)),
    (mul, arrow([int, int], int)),
    (div, arrow([int, int], int))
]).
arrow(_,_).

/* Programmes */
type_prog(prog(B)) :- 
    contexte_initial(G),
    type_block(G,B,void).

/* Blocks */
type_block(G,block(CS),void) :- 
    type_cmd(G,CS,void).

/* Suite de commandes */
type_cmd(G,stats(S,CS),void) :- 
    type_stat(G,S,void),
    type_cmd(G,CS,void). 
type_cmd(G,def(D,CS),void) :- 
    type_def(G,D,G2),
    type_cmd(G2,CS,void). 
type_cmd(G,stat(S),void) :- type_stat(G,S,void).

/* Definitions */
type_def(G,const(X,T,E),G2) :- 
    type_expr(G,E,T),
    append(G,[(X,T)],G2).
type_def(G,fun(X,T,L,E),G3) :- 
    append(G,L,G2),
    type_expr(G2,E,T),
    list_types(L,LT),
    append(G,[(X,arrow(LT,T))], G3).
type_def(G,funrec(X,T,L,E),G2) :- 
    list_types(L,LT),
    append(G,[(X,arrow(LT,T))], G2),
    append(G2,L,G3),
    type_expr(G3,E,T).
type_def(G,var(X,int),G2) :- 
    append(G,[(X,ref(int))], G2).
type_def(G,var(X,bool),G2) :- 
    append(G,[(X,ref(bool))], G2).
type_def(G,proc(X,L,B),G3) :- 
    maplist(funA, L, LR),
    append(G,LR,G2),
    type_block(G2,B,void),
    list_types(LR,LT),
    append(G,[(X,arrow(LT,void))], G3).
type_def(G,procrec(X,L,B),G2) :- 
    maplist(funA, L, LR),
    list_types(LR,LT),
    append(G,[(X,arrow(LT,void))], G2),
    append(G2,LR,G3),
    type_block(G3,B,void).

/* Intructions */
type_stat(G,echo(E),void) :- type_expr(G,E,int).
type_stat(G,set(X,E),void) :- 
    type_expar(G,X,ref(T)),
    type_expar(G,E,T). 
type_stat(G,if2(E,B1,B2),void) :- 
    type_expr(G,E,bool),
    type_block(G,B1,void),
    type_block(G,B2,void).
type_stat(G,while(E,B),void) :- 
    type_expr(G,E,bool),
    type_block(G,B,void).
type_stat(G,call(X,LE),void) :- 
    member((X,T),G),
    arrow(LT,void) = T,
    types_exprp_list(G,LE,LT).

/* Parametres d’appel */
type_expar(G,adr(X),ref(T)) :- member((X,ref(T)),G).
type_expar(G,E,T) :- type_expr(G,E,T).  

/* Expressions */
type_expr(_,num(_),int).
type_expr(G,ident(X),T) :- member((X,ref(T)),G).
type_expr(G,ident(X),T) :- member((X,T),G).
type_expr(G,if(E1,E2,E3),T) :- 
    type_expr(G,E1,bool),
    type_expr(G,E2,T),
    type_expr(G,E3,T).
type_expr(G,and(E1,E2),bool) :- 
    type_expr(G,E1,bool),
    type_expr(G,E2,bool).
type_expr(G,or(E1,E2),bool) :- 
    type_expr(G,E1,bool),
    type_expr(G,E2,bool).
type_expr(G,app(E,LE),T) :- 
    type_expr(G,E,TE),
    arrow(LT,T) = TE,
    types_expr_list(G,LE,LT).
type_expr(G,abs(L,E),arrow(LT,T)) :- 
    append(G,L,G2),
    type_expr(G2,E,T),
    list_types(L,LT).
type_expr(G,alloc(E),vec(_T)) :- type_expr(G,E,int).
type_expr(G,len(E),int) :- type_expr(G,E,vec(_)).
type_expr(G,nth(E1,E2),T) :- 
    type_expr(G,E1,vec(T)),
    type_expr(G,E2,int).
type_expr(G,vset(E1,E2,E3),vec(T)) :- 
    type_expr(G,E1,vec(T)),
    type_expr(G,E2,int),
    type_expr(G,E3,T).


/* AUX list_types : extrait les types d'une liste de paires (id, type) */
list_types([],[]).
list_types([(_,X)|T],[X|XS]) :- list_types(T,XS).
/* AUX types_expr_list : vérifie les types d'une liste d'expressions */
types_expr_list(_,[],[]).
types_expr_list(G, [E|LE], [T|LT]) :-
    type_expr(G, E, T),
    types_expr_list(G, LE, LT).
/* AUX A */
funA((var2(X),T),(X,ref(T))).
funA((X,T),(X,T)).
/* AUX types_exprp_list : vérifie les types d'une liste de param */
types_exprp_list(_,[],[]).
types_exprp_list(G, [E|LE], [T|LT]) :-
    type_expar(G, E, T),
    types_exprp_list(G, LE, LT).