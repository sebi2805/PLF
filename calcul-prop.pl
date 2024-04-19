vars(P, [P]) :- atom(P).
vars(not(P), R) :-
          vars(P, R).
vars(imp(P, Q), Res) :-
          vars(P, Res1), 
          vars(Q, Res2), 
          union(Res1, Res2, Res).

vars(and(P, Q), Res) :-
          vars(P, Res1), 
          vars(Q, Res2), 
          union(Res1, Res2, Res).

vars(or(P, Q), Res) :-
          vars(P, Res1), 
          vars(Q, Res2), 
          union(Res1, Res2, Res).


val(X, [(X, R)|_], R) :- !.
val(X, [(_, _)|T], R):-
          val(X, T, R).


bnon(1, 0):-!.
bnon(0, 1):-!.

band(1, 1, 1):-!.
band(_, _, 0):-!.

bor(0, 0, 0):-!.
bor(_, _, 1):-!.

bimp(1, 0, 0):-!.
bimp(_, _, 1):-!.

 
eval(X, List, Res) :- val(X, List, Res).
eval(non(P), List, Res) :-
    eval(P, List, ResP),
    bnon(ResP, Res).
eval(imp(P, Q), List, Res) :-
    eval(P, List, ResP),
    eval(Q, List, ResQ),
    bimp(ResP, ResQ, Res).
eval(and(P, Q), List, Res) :-
    eval(P, List, ResP),
    eval(Q, List, ResQ),
    band(ResP, ResQ, Res).
eval(or(P, Q), List, Res) :-
    eval(P, List, ResP),
    eval(Q, List, ResQ),
    bor(ResP, ResQ, Res).

evals(_, [], []).
evals(P, [H|T], [Res2|Res1]) :-
        evals(P, T, Res1),
        eval(P, H, Res2).  



cartesian_product([_], _, [], []):-!.
cartesian_product([_|T1], L2, [], Res) :-
          cartesian_product(T1, L2, L2, Res).
cartesian_product([H1|T1], L2, [H2|T2], [[H1, H2]|Res]) :-
          cartesian_product([H1|T1], L2, T2, Res).
          
cartesian_product(L1, L2, R):-
          cartesian_product(L1, L2, L2, R).

