% implementarea verificarii proprietatii unei formule
% de a fi tautologie

% vom reprezenta formulele prin predicatele
% non/1, and/2, or/2, imp/2

% ?- X = imp(p, or(q, r)).

% consideram ca variabilele propozitionale 
% coincid cu atomii din Prolog 

% Exercitiul 1 - vars/2
% determinam toate variabilele propozitionale
% dintr-o formula
% union/3 

vars(X, [X]) :- atom(X).
vars(non(P), Res) :-
    vars(P, Res).
vars(imp(P, Q), Res) :-
    vars(P, ResP),
    vars(Q, ResQ),
    union(ResP, ResQ, Res).
vars(and(P, Q), Res) :-
    vars(P, ResP),
    vars(Q, ResQ),
    union(ResP, ResQ, Res).
vars(or(P, Q), Res) :-
    vars(P, ResP),
    vars(Q, ResQ),
    union(ResP, ResQ, Res).

% vars(imp(and(p, r), or(p, q), LV).
% lV = [p, r, q]

% Exercitiul 2 - val/3 
% predicatul val primeste un atom propozitional
% primeste o evaluare e
% intoarce valoarea atomului in aceasta evaluare

% vom tine evaluarea sub forma unei liste de perechi
% [(a, 0), (p, 1), (q, 0), (b, 0)]

% ?- val(a, [(a, 1), (b, 0)], X).
% X = 1

% ?- val(b, [(a, 1), (b, 0)], X).
% X = 0

val(X, [(X, E) | _], E) :- !.
val(X, [_ | T], E) :- val(X, T, E).


% Exercitiul 3 - implementarea tabelelor de adevar elementare
% bnon/2, bor/3, band/3, bimp/3

bnon(0, 1) :- !.
bnon(1, 0) :- !.
bor(0, 0, 0) :- !.
bor(0, 1, 1) :- !.
bor(1, 0, 1) :- !.
bor(1, 1, 1) :- !.
bimp(P, Q, Res) :-
    bnon(P, NonP),
    bor(NonP, Q, Res).
% p -> q = (~p) \/ q
band(P, Q, Res) :-
    bnon(P, NonP),
    bnon(Q, NonQ),
    bor(NonP, NonQ, NonRes),
    bnon(NonRes, Res).
    
% p /\ q = ~((~p) \/ (~q))


% ?- bnon(1, X).
% X = 0

% ?- bimp(1, 0, X).
% X = 0

% Exercitiul 4 - evaluarea e+ a unei formule 
% eval/3
% eval(+Form, +Eval, -FormEval).
% ?- eval(imp(p, or(q, r)), [(p, 0), (q, 1), (r, 0)], R).
% R = 1 

eval(X, List, Val) :- val(X, List, Val).
eval(non(P), List, Val) :-
    eval(P, List, ValP),
    bnon(ValP, Val).
eval(imp(P, Q), List, Val) :-
    eval(P, List, ValP),
    eval(Q, List, ValQ),
    bimp(ValP, ValQ, Val).
eval(and(P, Q), List, Val) :-
    eval(P, List, ValP),
    eval(Q, List, ValQ),
    band(ValP, ValQ, Val).
eval(or(P, Q), List, Val) :-
    eval(P, List, ValP),
    eval(Q, List, ValQ),
    bor(ValP, ValQ, Val).


% Exercitiul 5 - evaluarea formulei in mai multe evaluari
% evals/3
% evals(+Form, +ListEval, -ListRes).
% ?- evals(imp(p, or(q, r)), [[(p, 0), (q, 1), (r, 0)], [(p, 1), (q, 0), (r,0)]], ListRes).
% ListRes = [1, 0]

evals(_, [], []) :- !.
evals(Form, [H | T], [FormEval | TEval]) :-
    eval(Form, H, FormEval),
    evals(Form, T, TEval).

% Exercitiul 6 - generarea tuturor evaluarilor posibile (generarea tabelului de adevar)
% evs/2
% evs(+ListVarProp, -ListEval)

cartesian_product([], _, [_], []) :- !.
cartesian_product([], L1, [_ | T2], R) :-
    cartesian_product(L1, L1, T2, R).
cartesian_product([H1 | T1], L1, [H2 | T2], [HR | TR]) :-
    append(H1, H2, HR),
    cartesian_product(T1, L1, [H2 | T2], TR).

cartesian_product(L1, L2, R) :- cartesian_product(L1, L1, L2, R).

repeat_cartesian_product(L, 1, L) :- !.
repeat_cartesian_product(L, N, Result) :- 
    cartesian_product(L, [[0],[1]], TempResult),
    N1 is N - 1,
    repeat_cartesian_product(TempResult, N1, Result).

zip([], _, []) :- !.
zip(_, [], []) :- !.
zip([H1 | T1], [H2 | T2], [(H1, H2) | TR]) :-
    zip(T1, T2, TR).

ziplist(_, [], []) :- !.
ziplist(Vars, [H | T], [HR | TR]) :-
    zip(Vars, H, HR),
    ziplist(Vars, T, TR).

evs(Vars, Result) :-
	length(Vars, N),
    repeat_cartesian_product([[0],[1]], N, ListEval),
    ziplist(Vars, ListEval, Result).
% evs([c,d], ListEval).
% ListEval = [[(c, 0), (d, 0)],[(c, 1), (d, 0)],[(c, 0), (d, 1)],[(c, 1), (d, 1)]]

% Exercitiul 7 - evaluarea unei formule in tot tabelul de adevar 
% all_evals(+Form, -ListEval)
% ?- all_evals(imp(p, q), List).
% List = [1, 0, 1, 1]

all_evals(Form, Result) :-
    vars(Form, Vars),
    evs(Vars, ListEval),
    evals(Form, ListEval, Result).

% Exercitiul 8 - taut/1 
% taut(+Form). si intoarce true daca e tautologie si false altfel
% este tautologie daca toate valorile sunt 1 in lista tuturor evaluarilor din tabelul de adevar

all_eq_1([1]) :- !.
all_eq_1([1 | T]) :- all_eq_1(T).

taut(Form) :-
    all_evals(Form, ListEval),
    all_eq_1(ListEval).

% ?- taut(imp(p, p)).
% true 

% ?- taut(imp(p, imp(q, p)).
% true 

% ?- taut(imp(p, q)).
% false 



