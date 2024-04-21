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


% remove_duplicate([], []).
% remove_duplicate([H|T], [H|R]):-
%     remove_duplicate(T, R),
%     \+ element_of(R, H). 
% remove_duplicate([H|T], R):-
%     remove_duplicate(T, R),
%     element_of(R, H). 


s(1, 2).
s(1, 3).
s(2, 4).
s(2, 5).
s(3, 5).
s(3, 4).
objective(5).


extend([Node | NodeTail], L) :-
    findall([NewNode, Node | NodeTail], (s(Node, NewNode)), L).

bfs([[H|T]|_], [H|T]) :- objective(H).
bfs([Path|PathTail], R) :-
    extend(Path, ExtendedPath),
    append(ExtendedPath, PathTail, NewPaths),
    bfs(NewPaths, R).

solve(Start, R):-
    bfs([[Start]], R).

def(myTree, tree(1, tree(2, tree(4, nil, tree(7, nil, nil)), nil), tree(3, tree(5, nil, nil), tree(6, nil, nil)))).

inorder(nil, []).
inorder(tree(Leaf, nil, nil), [Leaf]):-!.
inorder(tree(Root, Left, Right), R) :-
    inorder(Left, LeftList), 
    inorder(Right, RightList), 
    append(LeftList, [Root|RightList], R).

preorder(nil, []).
preorder(tree(Leaf, nil, nil), [Leaf]):-!.
preorder(tree(Root, Left, Right), R) :-
    preorder(Left, LeftList), 
    preorder(Right, RightList), 
    append([Root | LeftList], RightList, R).

postorder(nil, []).
postorder(tree(Leaf, nil, nil), [Leaf]):-!.
postorder(tree(Root, Left, Right), R) :-
    postorder(Left, LeftList), 
    postorder(Right, RightList), 
    append(LeftList, RightList, R1),
    append(R1, [Root], R).

srd(nil, []).
srd(tree(Root, Left, Right), Result) :-
    srd(Left, ResultLeft),
    srd(Right, ResultRight),
    append(ResultLeft, [Root | ResultRight], Result).


compare_scores(Order, (_, Score1), (_, Score2)) :-
    compare(Order, Score2, Score1).

sort_by_score(UnsortedList, SortedList) :-
    predsort(compare_scores, UnsortedList, SortedList).


min([H], H).
min([H|T], H) :-
    min(T, Min),
    H < Min,
    !.
min([_|T], Min):-
    min(T, Min).

eliminate(_, [], []):-!.
eliminate(E, [E|T], L):-
    eliminate(E, T, L),
    !.
eliminate(E, [H|T], [H|L]):-
    eliminate(E, T, L).

selection_sort([], []):-!.
selection_sort([H|T], [Min|MinTail]):-
    min([H|T], Min), 
    eliminate(Min, [H|T], NewList),
    selection_sort(NewList, MinTail) .

element_at(H, [H|_], 0):-!.
element_at(X, [_|T], K):-
    K1 is K-1,
    element_at(X, T, K1).

remove_duplicate([], []).
remove_duplicate([H|T], [H|R]) :-
    eliminate(H, T, NewTail),
    remove_duplicate(NewTail, R).

rotate_aux([], Acc, Acc):-!.
rotate_aux([H|T], Acc, R):-
    rotate_aux(T, [H|Acc], R).
rotate(L, R):-
    rotate_aux(L, [], R).


list_compare(Order, List1, List2):-
    length(List1, L1),
    length(List2, L2),
    compare(Order, L1, L2).
sort_lists(UnorderedLists, OrderedLists):-
    predsort(list_compare, UnorderedLists, OrderedLists).


flatten([], []).
flatten([H|T], R):-
    is_list(H),
    flatten(H, R1),
    flatten(T, R2),
    append(R1, R2, R), !.
flatten([H|T], [H|R]):-
    flatten(T, R).



select_e(L, E, Rest):-
    member(E, L),
    eliminate(E, L, Rest).

permutari([], []).
permutari(L, [E|Rez]):-
    select_e(L, E, Rest), 
    permutari(Rest, Rez).

solve_permutari(L, R):-
    findall(X, (permutari(L, X)), R).

 def1(myBinarySearchTree, tree(5, tree(3, tree(2, nil, nil), tree(4, nil, nil)), tree(7, tree(6, nil, nil), tree(8, nil, nil)))).   


binary_search(tree(Root, _, _), Root, [Root]).
binary_search(tree(Root, Left, _), Value, [Root|List]) :-
        Root > Value, 
        binary_search(Left, Value, List). 
binary_search(tree(Root, _, Right), Value, [Root|List]) :-
        Root < Value, 
        binary_search(Right, Value, List). 
solve_binary_search(Value, Rez):-
    def1(myBinarySearchTree, X), 
    binary_search(X, Value, Rez).


rotire([H|T], RL):-
    append(T, [H], RL).

rotire_k(RL, 0, RL):-!.
rotire_k(L, K, R):-
    rotire(L, RL), 
    K1 is K-1,
    rotire_k(RL, K1, R).

num_to_list(0, []):-!.
num_to_list(N, [D|R]):-
    D is N mod 10,
    NewN is N//10,
    num_to_list(NewN, R).

is_palindrome(N):-
    num_to_list(N, L),
    reverse(L, L).


cmmdc(A, B, Rez) :-
    