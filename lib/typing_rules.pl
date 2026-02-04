
main :- read(user_input, X), type_check(X).

/*type_check(prog(P)) :- type_prog(prog(P)), write("OK\n").*/
type_check(_) :- write("KO\n").
/*
contexte_initial([_
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

type_prog(prog(CS), void) :- 
    contexte_initial(G),
    type_cmd(G,CS,void).

type_expr(_,num(_),int).

type_cmd(G,stat(S),void) :- type_stat(G,S,void).
type_cmd(G,stat(S),void) :- type_stat(G,S,void).
type_stat(G,echo(E),void) :- type_expr(G,E,int).

*/