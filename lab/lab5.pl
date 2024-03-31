% Gaseste toti divizorii ca perechi 
divisionPair(A, B, R) :-
    numlist(A, B, Range),
    findall((X, Y), (member(X, Range), member(Y, Range), X mod Y =:=0), R).

zip([], _, []):-!.
zip(_, [], []):-!.
zip([H1|T1], [H2|T2], [(H1, H2)|TR]):-
    zip(T1, T2, TR).

oddIndexes(L, LR) :-
    length(L, Length),
    numlist(0, Length, Range),
    zip(L, Range, ZipList),
    findall(X, (member((X, Y), ZipList), Y mod 2 =:= 1), LR).


s(1, 2).
s(1, 3).
s(2, 4).
s(2, 5).
s(3, 5).
s(3, 4).
objective(5).

extend([N|Path], Result):-
    findall([NewN, N|Path], (s(N, NewN), not(member(NewN, [N|Path]))), Result), 
    !.
%vrem sa oprim recursia 
extend(_, []).


breadthFirst([[N|Path] | _], [N|Path]):- objective(N).
breadthFirst([ Path | PathTail], Res):-
    extend(Path, ExtendedPath),
    append(PathTail, ExtendedPath, NewPath),
    breadthFirst(NewPath, Res).

solve(Start, R):-   
    findall(S, breadthFirst([[Start]], S), R).



right(house(X, _, _, _, _, _), house(Y, _, _, _, _, _), Street) :-
    nextto(house(Y, _, _, _, _, _), house(X, _, _, _, _, _), Street).

near(X, Y, Street) :-
    nextto(X, Y, Street);
    nextto(Y, X, Street).
 

% house(Number,Nationality,Colour,Pet,Drink,Cigarettes)


solution(Street, ZebraOwner) :-
    Street = [
        house(1, _, _, _, _, _),
        house(2, _, _, _, _, _),
        house(3, _, _, _, _, _),
        house(4, _, _, _, _, _),
        house(5, _, _, _, _, _)
    ],
    member(house(_,english,red,_,_,_), Street),
    member(house(_,spanish,_,dog,_,_), Street),
    member(house(_,_,green,_,coffee,_), Street),
    member(house(_,ukraininan,_,_,tea,_), Street),

    right(house(_, _, green, _, _, _), house(_, _, beige, _, _, _), Street),
    member(house(_, _, _, snails, _, old_gold), Street),
    member(house(_, _, yellow, _, _, kools), Street),
    Street = [_, _, house(_, _, _, _, milk, _), _, _],
    Street = [house(_, norwegian, _, _, _, _)|_],

    near(house(_, _, _, _, _, chesterfields), house(_, _, _, fox, _, _), Street),
    near(house(_, _, _, _, _, kools), house(_, _, _, horse, _, _), Street),
    member(house(_, _, _, _, orange_juice, lucky_strike), Street),
    member(house(_, japanese, _, _, _, parliaments), Street),

    near(house(_, norwegian, _, _, _, _), house(_, _, blue, _, _, _), Street),
    member(house(_, ZebraOwner, _, zebra, _, _), Street).



/**
        5
       / \
      3   8
     / \   \
    1   4   9
**/

tree(5, 3, 8).
tree(3, 1, 4).
tree(1, nil, nil).
tree(4, nil, nil).
tree(8, nil, 9).
tree(9, nil, nil).

arbore_bin( tree(5, 
                 tree(3, 
                      tree(1, nil, nil), 
                      tree(4, nil, nil)
                     ), 
                 tree(8, 
                      nil, 
                      tree(9, nil, nil)
                     )
                ) 
          ).

inorder(nil, []). % Cazul bază: arborele vid nu are noduri de vizitat.
inorder(tree(Root, Left, Right), List) :-
    inorder(Left, LList), % Vizitează subarborele stâng
    inorder(Right, RList), % Vizitează subarborele drept
    append(LList, [Root|RList], List). % Construiește lista finală

inorder_aux(R, L) :-
    arbore_bin(tree(R, C1, C2)),
    inorder(tree(R, C1, C2), L).
    
preorder(nil, []). % Cazul bază: arborele vid nu are noduri de vizitat.
preorder(tree(Root, Left, Right), [Root|List]) :-
    preorder(Left, LList), % Vizitează subarborele stâng
    preorder(Right, RList), % Vizitează subarborele drept
    append(LList, RList, List). % Construiește lista finală

% Post-ordine
postorder(nil, []). % Cazul bază: arborele vid nu are noduri de vizitat.
postorder(tree(Root, Left, Right), List) :-
    postorder(Left, LList), % Vizitează subarborele stâng
    postorder(Right, RList), % Vizitează subarborele drept
    append(LList, RList, TempList),
    append(TempList, [Root], List). % Construiește lista finală

leaves_aux(nil, []).
leaves_aux(tree(R, nil, nil), [R]).
leaves_aux(tree(_, C1, C2), L) :-
    leaves_aux(C1, L1),
    leaves_aux(C2, L2),
    append(L1, L2, L).
leaves(R, L):-
    arbore_bin(tree(R, C1, C2)),
    leaves_aux(tree(R, C1, C2), L).

/**
divs_aux(N, N, [N]).
divs_aux(N, D, [D|L]):-
    N mod D =:= 0,
    D1 is D+1, 
    divs_aux(N, D1, L).
divs_aux(N, D, L) :-
    D1 is D+1, 
    divs_aux(N, D1, L).

divs(N, L):-
    divs_aux(N, 1, L).
    **/

divs(N, L) :-
    numlist(1, N, Range),
    findall(X, ( member(X, Range), N mod X =:= 0 ), L).

divs_pair(N, L) :-
    numlist(1, N, Range),
    findall((A, B), ( member(A, Range), N mod A =:= 0, B is N/A ), L).


