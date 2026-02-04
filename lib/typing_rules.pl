
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
type_prog(prog(CS),void) :- 
    contexte_initial(G),
    type_cmd(G,CS,void).

/* Suite de commandes */
type_cmd(G,cmd(def(D),CS),void) :- 
    type_def(G,D,G2),
    type_cmd(G2,CS,void). 
type_cmd(G,stat(S),void) :- type_stat(G,S,void).

/* Definitions */
type_def(G,const(X,T,E),G2) :- 
    type_expr(G,E,T),
    append(G,[(X,T)],G2).
/* FUN a faire */ 
types()
type_def(G,fun(X,T,L,E),G3) :- 
    append(G,L,G2),
    type_expr(G2,E,T),
    append(G,[(X,arrow(types(L),T))], G3).

/* FUN REC */

/* Intruction */
type_stat(G,echo(E),void) :- type_expr(G,E,int).

/* Expressions */
type_expr(_,num(_),int).
type_expr(G,ident(X),T) :- member((X,T),G).
type_expr(G,if(E1,E2,E3),T) :- 
    member((E1,bool),G),
    type_expr(G,E2,T),
    type_expr(G,E3,T).
type_expr(G,and(E1,E2),bool) :- 
    type_expr(G,E1,bool),
    type_expr(G,E2,bool).
type_expr(G,or(E1,E2),bool) :- 
    type_expr(G,E1,bool),
    type_expr(G,E2,bool).

/* APP */
/* ABS */