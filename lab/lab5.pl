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


tree(1, 2, 3).
tree(2, nil, nil).
tree(3, nil, nil).

