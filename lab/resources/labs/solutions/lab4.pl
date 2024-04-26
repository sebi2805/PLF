% Laboratorul 4

% Exercitiul 1 

% Se rezolva unificarile aplicand algoritmul de unificare.
% In Prolog, avem predicatul unify_with_occurs_check/2 

% unify_with_occurs_check(f(h(a), g(X)), f(Y, Y)). <- se obtine false 
% variabilele din limbaj sunt variabilele din Prolog si se scriu cu Majuscula! 

% Exercitiul 2

word(lovable).
word(energy).
word(savage).
word(whiskers).
word(agile).
word(purring).

list_pos([Result | _], Index, Index, Result) :- !.
list_pos([_ | T], CurrentIndex, Index, Result) :-
    NewIndex is CurrentIndex + 1,
    list_pos(T, NewIndex, Index, Result).

list_pos(List, Index, Result) :- 
    list_pos(List, 0, Index, Result).

all_different([]) :- !.
all_different([H | T]) :-
    not(member(H, T)),
    all_different(T).

string_chars_list([], []) :- !.
string_chars_list([HW | TW], [HL | TL]) :-
    string_chars(HW, HL),
    string_chars_list(TW, TL).

intersection(L1, L2, Pos1, Pos2) :-
    list_pos(L1, Pos1, Intersection),
    list_pos(L2, Pos2, Intersection).

extract_indices([], _, []) :- !.
extract_indices([HL | TL], [HI | TI], [HR | TR]) :-
    list_pos(HL, HI, HR),
    extract_indices(TL, TI, TR).

solution(W1, W2, W3, W4, W5, W6, L) :-
    word(W1), word(W2), word(W3), word(W4), word(W5), word(W6),
    all_different([W1, W2, W3, W4, W5, W6]),
    string_chars_list([W1, W2, W3, W4, W5, W6], [L1, L2, L3, L4, L5, L6]),
    intersection(L1, L6, 4, 0),
    intersection(L2, L6, 5, 1),
    intersection(L3, L6, 6, 2),
    intersection(L4, L6, 6, 3),
    intersection(L5, L6, 4, 4),
    extract_indices([L1, L2, L3, L4, L5], [1, 2, 3, 3, 1], L), !.

% Exercitiul 3

connected(1,2).
connected(3,4).
connected(5,6).
connected(7,8).
connected(9,10).
connected(12,13).
connected(13,14).
connected(15,16).
connected(17,18).
connected(19,20).
connected(4,1).
connected(6,3).
connected(4,7).
connected(6,11).
connected(14,9).
connected(11,15).
connected(16,12).
connected(14,17).
connected(16,19).

path(X, Y) :- connected(X, Y).
path(X, Y) :- 
    path(X, Z),
    connected(Z, Y).
