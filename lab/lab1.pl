male(sam).
male(oliver).
male(ben).
male(peter).
male(roger).

female(elizabeth).
female(sandra).
female(mary).
female(lisa).
female(olivia).

parent_of(sandra, sam).
parent_of(sandra, elizabeth).
parent_of(ben, sam).
parent_of(ben, elizabeth).
parent_of(peter, oliver).
parent_of(peter, sandra).
parent_of(roger, ben).
parent_of(roger, lisa).
parent_of(olivia, roger).
parent_of(olivia, mary).

mother_of(X, Y) :-
    female(X),
    parent_of(X, Y).

brother_of(X, Y) :-
    male(X),
    parent_of(Z, X),
    parent_of(Z, Y),
    X \= Y.

sister_of(X, Y) :-
    female(X),
    parent_of(Z, X),
    parent_of(Z, Y),
    X \= Y.

uncle_of(X, Y) :-
    male(X),
    brother_of(X, Z),
    parent_of(Z, Y).

aunt_of(X, Y) :-
    female(X),
    sister_of(X, Z),
    parent_of(Z, Y).

grandfather_of(X, Y) :-
    male(X),
    parent_of(X, Z),
    parent_of(Z, Y).

grandmother_of(X, Y) :-
    female(X),
    parent_of(X, Z),
    parent_of(Z, Y).

% Exercitiul 2
ancestor_of(X, Y) :- parent_of(X, Y).
ancestor_of(X, Y) :- parent_of(X, Z), ancestor_of(Z, Y).

% Exercitiul 3
distance((X1, Y1), (X2, Y2), D) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    D is sqrt(DX^2 + DY^2).

% Exercitiul 4
write_n(0, _).
write_n(N, Sign) :-
    N > 0,
    write_n_aux(N, Sign),
    nl,
    N1 is N - 1,
    write_n(N1, Sign).

write_n_aux(0, _).
write_n_aux(N, Sign) :-
    N > 0,
    write(Sign),
    N1 is N - 1,
    write_n_aux(N1, Sign).