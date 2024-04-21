% non/1, and/2, or/2, imp/2
% non(p), and(p, q), or(p, q), imp(p, q)
% or(p, imp(q, r))

% scop laborator: evaluarea formulelor astfel incat sa decidem daca o formula este tautologie
% adica sa fie adevarata in orice evaluare 

% vom implementa algoritmic, simuland tabelele de adevar 

% Exercitiul 1 - obtinem variabilele dintr-o formula

% vars(imp(p, or(q, r)).
% [p, q, r]

% reamintire seminar - principiul recursiei pe formule 
% p -> [p]
% non(p) -> [p]
% and(p, or(p, r)) -> [p] U [p, r] -> [p, r]

vars(X, [X]) :- atom(X).
vars(non(X), R) :- vars(X, R).
vars(and(P, Q), R) :-
    vars(P, RP),
    vars(Q, RQ),
    union(RP, RQ, R).
vars(or(P, Q), R) :-
    vars(P, RP),
    vars(Q, RQ),
    union(RP, RQ, R).
vars(imp(P, Q), R) :-
    vars(P, RP),
    vars(Q, RQ),
    union(RP, RQ, R).

% Exercitiul 2 - evaluarea atomilor propozitionali intr-o evaluare
% e : Var -> {0, 1}
% [(a, 0), (b, 1), (c, 0), (d, 0)]
% val(b, [(a, 0), (b, 1), (c, 0), (d, 0)], Val).
% Val = 1
% val(c, [(a, 0), (b, 1), (c, 0), (d, 0)], Val).
% Val = 0

val(X, [(X, E) | _], E) :- !.
val(X, [_ | T], Res) :-
    val(X, T, Res).

% diferenta dintre e si e+ este ca e : Var -> {0, 1}, iar e+ : Form -> {0, 1}
% e+ extinde e (si am vazut la frumosul seminar de luni ca e+ este unic!!!!!!)

% Exercitiul 3 - implementam tabelele de adevar pentru operatorii simpli 
% bnon/2, bor/3, band/3, bimp/3
% bnon(+I, -O), bor(+I1, +I2, -O)
% bnon(1, A).
% A = 0
% band(0, 0, A).
% A = 0
% bor(0, 1, B).
% B = 1 

bnon(0, 1).
bnon(1, 0).
bor(0, 0, 0).
bor(1, _, 1).
bor(_, 1, 1).
bimp(P, Q, Res) :-
    bnon(P, NonP),
    bor(NonP, Q, Res).
band(P, Q, Res) :-
    bnon(P, NonP),
    bnon(Q, NonQ),
    bor(NonP, NonQ, NonRes),
    bnon(NonRes, Res).

% P /\ Q := ~( (   ~P) \/ (   ~Q))

% Exercitiul 4 - implementam e+
% eval(Form, Eval, Val)
% eval(imp(a, b), [(a, 0), (b, 1)], Val).
% Val = 1 
% eval(imp(a, or(b, c)), [(a, 1), (b, 0), (c, 1)], Val).
% Val = ...

eval(X, List, R) :- val(X, List, R).
eval(non(P), List, R) :-
    eval(P, List, TempResult),
    bnon(TempResult, R).
eval(or(P, Q), List, R) :-
    eval(P, List, TempP),
    eval(Q, List, TempQ),
    bor(TempP, TempQ, R).
eval(and(P, Q), List, R) :-
    eval(P, List, TempP),
    eval(Q, List, TempQ),
    band(TempP, TempQ, R).
eval(imp(P, Q), List, R) :-
    eval(P, List, TempP),
    eval(Q, List, TempQ),
    bimp(TempP, TempQ, R).
    
% e+(~phi) = ~e+(phi)

% Exercitiul 5 - evaluam formula in mai multe evaluari
% evals(Form, ListEval, ListRes)
% evals(imp(a, b), [[(a, 0), (b, 1)], [(a,1), (b, 0)]], LR).
% LR = [1, 0]

evals(_, [], []) :- !.
evals(Form, [HE | TE], [HRes | TRes]) :-
    eval(Form, HE, HRes),
    evals(Form, TE, TRes).

% Exercitiul 6 - obtinem tot tabelul de adevar, plecand de la variabilele propozitionale
% evs([a, b], LR).
% LR = [[(a, 0), (b, 0)], [(a, 1), (b, 0)], [(a, 0), (b, 1)], [(a, 1), (b, 1)]]

% [0,1] x [0, 1] => (0, 0), (0, 1) ...
% [[0], [1]] x [[0], [1]] => [[0, 0], [1, 0], [0, 1], [1, 1]]
% construim un produs cartezian 
% 2 -> [[0, 0], [1, 0], [0, 1], [1, 1]]
% 3 -> [[0, 0, 0], [1, 0, 0], ..., [1, 1, 1]]
% [[(a,0), (b,0), (c,0)], [(a,1), (b,0), (c,0)], ..., [(a,1), (b,1), (c,1)]]

% [0, 1] x [a, b, c] => [(0,a), (1,a), (0, b), (1, b), (0, c), (1,c)]
% [0, 1] x [a, b, c] => (0, a)
%    [1] x [a, b, c] =>        (1, a)
% [0, 1] x   [b, c]  =>                 (0, b)
%    [1] x   [b, c] =>                         (1, b)
% [0, 1] x       [c] =>                                 (0, c)
%    [1] x       [c] =>                                          (1, c)
%    [] x        [_]

cartesian_product([], _, [_], []).
cartesian_product([], L, [_ | T], R) :-
	cartesian_product(L, L, T, R).
cartesian_product([H1 | T1], L1, [H2 | T2], [HR | TR]) :-
    append(H1, H2, HR),
    cartesian_product(T1, L1, [H2 | T2], TR).

cartesian_product(L1, L2, R) :- cartesian_product(L1, L1, L2, R).

rep_cartesian_product(L, 1, L) :- !.
rep_cartesian_product(L, N, Res) :-
    cartesian_product(L, [[0], [1]], TempRes),
    N1 is N - 1,
    rep_cartesian_product(TempRes, N1, Res).

zip([], _, []).
zip(_, [], []).
zip([H1 | T1], [H2 | T2], [(H1, H2) | TR]) :-
    zip(T1, T2, TR).

ziplist(_, [], []).
ziplist(Vars, [H | T], [HR | TR]) :-
    zip(Vars, H, HR),
    ziplist(Vars, T, TR).


evs(Var, Result) :-
    length(Var, N),
    rep_cartesian_product([[0],[1]], N, ListEval),
    ziplist(Var, ListEval, Result).
% Exercitiul 7 - all_evals: evaluam formula in toate evaluarile posibile 
% all_evals(imp(a, a), LR).
% LR = [1, 1]
% all_evals(imp(a, b), LR).
% [1,0,1,1]

all_evals(Form, Result) :-
    vars(Form, Vars),
    evs(Vars, ListEval),
    evals(Form, ListEval, Result).


% Exercitiul 8 - verificam daca o formula e tautologie, taut/1
% apeleaza all_evals, si trebuie sa obtina 1 pe fiecare evaluare 
% taut(imp(a,a)).
% true 

% taut(imp(a,b)).
% false 

all_1([1]).
all_1([1 | T]) :- all_1(T).

taut(Form) :-
    all_evals(Form, ListEval),
    all_1(ListEval).

% Exercitiul 9 - definiti un predicat care sa fie true cand 
% formula P primita este SATISFIABILA

% sat/1 

at_least_1([1 | _]).
at_least_1([_ | T]) :- at_least_1(T).

sat(Form) :-
    all_evals(Form, ListEval),
    at_least_1(ListEval).