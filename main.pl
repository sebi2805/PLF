list_length([], 0).
list_length([_|T], L) :- list_length(T, L1), L is L1 + 1.


list_sum([], 0).
list_sum([H|T], S) :- list_sum(T, S1), S is S1 + H.


elements_of(X, [X|_]).
elements_of(X, [_|T]) :- elements_of(X, T).

all_a([]).
all_a([ a | T]) :- all_a(T).


all_a([ a | T]) :- all_a(T).

trans_a_b([], []).
trans_a_b([a| T1], [b|T2]) :-
    trans_a_b(T1, T2).


scalarMult(_, [], []).
scalarMult(X, [H|T], [H1|T1]) :- H1 is X * H, scalarMult(X, T, T1).


dot([], [], 0).
dot([H1|T1], [H2| T2], Res) :-dot(T1, T2, Res1),  Res is H1 * H2 + Res1.


max([], 0).
