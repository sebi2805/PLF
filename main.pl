male(sam).
male(oliver).
male(ben).
male(peter).
male(roger).
male(sebi).
female(elizabeth).
female(sandra).
female(mary).
female(lisa).
female(olivia).
parent_of(sandra, sam).
parent_of(sandra, elizabeth).
parent_of(sebi, sam).
parent_of(ben, sam).
parent_of(ben, elizabeth).
parent_of(peter, oliver).
parent_of(peter, sandra).
parent_of(roger, ben).
parent_of(roger, lisa).
parent_of(olivia, roger).
parent_of(olivia, mary).


father_of(C, P) :-
    parent_of(C, P), 
    male(P).

mother_of(C, P) :-
    parent_of(C, P), 
    female(P).

brother_of(C, B) :-
    parent_of(C, P),
    parent_of(B, P), 
    B \= C, 
    male(B).

sister_of(C, S) :-
    parent_of(C, P),
    parent_of(S, P), 
    S \= C, 
    female(S).

uncle_of(C, U) :-
    parent_of(C, P),
    brother_of(P, U).

aunt_of(C, U) :-
    parent_of(C, P),
    sister_of(P, U).

grandfather_of(C, G) :-
    parent_of(C, P),
    parent_of(P, G),
    male(G).

grandmother_of(C, G) :-
    parent_of(C, P),
    parent_of(P, G),
    female(G).

% exercitiul 2

ancestor_of(C, P) :-
    parent_of(C, P).
ancestor_of(C, P) :-
    parent_of(C, P1),
    ancestor_of(P1, P).

distance((X1, Y1), (X2, Y2), R) :-
    R is sqrt((X1-X2)**2+(Y1-Y2)**2).

write_aux(1, Sym) :-
    write(Sym),
    write('\n'),
    !.
write_aux(N, Sym) :-
    write(Sym),
    N1 is N-1,
    write_aux(N1, Sym).

write_n(0, _):-!.
write_n(N, Sym) :-
    write_aux(N, Sym),
    N1 is N-1,
    write_n(N1, Sym).

write_aux_2(N, I, Sym) :-.


write_n_2(N, Sym) :- .

min(X, Y, Y) :-
    X > Y,
    !.
min(X, _, X). 

triangle((X1, Y1), (X2, Y2), (X3, Y3)) :-
    distance((X1, Y1), (X2, Y2), A),
    distance((X1, Y1), (X3, Y3), B),
    distance((X2, Y2), (X3, Y3), C),
    (   A^2 =:= B^2 + C^2
    ;   B^2 =:= A^2 + C^2
    ;   C^2 =:= A^2 + B^2
    ), 
    !.

fib_aux(0, _, R, R) :- !.
fib_aux(N, E1, E2, R) :-
    E3 is E1+E2,
    N1 is N-1,
    fib_aux(N1, E2, E3, R).
fib(N, R) :- 
    fib_aux(N, 0, 1, R).

list_length([], 0):-!.
list_length([_|T], R):-
    list_length(T, R1),
    R is R1+1.

list_sum([], 0):-!.
list_sum([H|T], R):-
    list_sum(T, R1),
    R is R1+H.

element_of([H|_], H):-!.
element_of([_|T], R):-
    element_of(T, R).

all_a([]).
all_a([a|T]) :-
    all_a(T).

trans_a_b([], []).
trans_a_b([a|T], [b|R]):-
    trans_a_b(T, R).

scalarMult(_, [], []).
scalarMult(X, [H|T], [H1|R]):-
    scalarMult(X, T, R),
    H1 is H*X.

max_aux([], Max, Max):-!.
max_aux([H|T], Max, R) :-
    H>Max, 
    max_aux(T, H, R).
max_aux([H|T], Max, R):-
    H<Max,
    max_aux(T, Max, R).
    
max([H|T], R):-
    max_aux(T, H, R).

concat_lists([], [], []).
concat_lists([], [H|T], [H|R]):-
    concat_lists([], T, R).
concat_lists([H|T], L, [H|R]):-
    concat_lists(T, L, R).


remove_duplicate([], []).
remove_duplicate([H|T], [H|R]):-
    remove_duplicate(T, R),
    \+ element_of(R, H). 
remove_duplicate([H|T], R):-
    remove_duplicate(T, R),
    element_of(R, H). 



bagof1(T, Goal, List):-
    findall(X, (Goal), List).
