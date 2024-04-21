% Laboratorul 5

% Exercitiul 1

divisorsPairs(A, B, Result) :-
    numlist(A, B, Range),
    findall((X, Y), (member(X, Range), member(Y, Range), X mod Y =:= 0), Result).

zip([], _, []) :- !.
zip(_, [], []) :- !.
zip([H1 | T1], [H2 | T2], [(H1, H2) | TR]) :-
    zip(T1, T2, TR).

oddIndexes(List, Result) :-
    length(List, Length),
    numlist(0, Length, Range),
    zip(List, Range, ZippedList),
    findall(X, (member((X, Y), ZippedList), Y mod 2 =:= 1), Result).

% Exercitiul 2

s(1, 2).
s(1, 3).
s(2, 4).
s(2, 5).
s(3, 5).
s(3, 4).
objective(5).


extend([Node | Path], NewPath) :-
    findall([NewNode, Node | Path], (s(Node, NewNode), not(member(NewNode, [Node | Path]))), NewPath).

breadthfirst([[Node | Path] | _], [Node | Path]) :- objective(Node).
breadthfirst([Path | PathTail], Solution) :-
    extend(Path, ExtendedPath),
    append(ExtendedPath, PathTail, NewPath),
    breadthfirst(NewPath, Solution).

solve(Start, Solution) :-
    findall(S, breadthfirst([[Start]], S), Solution).
            
% Exercitiul 3 

% Zebra-puzzle

right(X, Y) :- X is Y + 1.
left(X, Y) :- right(Y, X).
near(X, Y) :- left(X, Y) , !.
near(X, Y) :- right(X, Y).

% house(Number,Nationality,Colour,Pet,Drink,Cigarettes)

solution(Street, ZebraOwner) :-
  	Street = [
    	house(1,_,_,_,_,_),
    	house(2,_,_,_,_,_),
    	house(3,_,_,_,_,_),
    	house(4,_,_,_,_,_),
    	house(5,_,_,_,_,_)
  	],
  	member(house(_,english,red,_,_,_), Street),
  	member(house(_,spanish,_,dog,_,_), Street),
  	member(house(X,_,green,_,coffee,_), Street),
  	member(house(_,ukrainian,_,_,tea,_), Street),
  	member(house(Y,_,beige,_,_,_), Street),
    right(X, Y),
    member(house(_,_,_,snails,_,oldGold), Street),
    member(house(U,_,yellow,_,_,kools), Street),
    member(house(3,_,_,_,milk,_), Street),
    member(house(1,norwegian,_,_,_,_), Street),
    member(house(A,_,_,_,_,chesterfields), Street),
    member(house(B,_,_,fox,_,_), Street),
    near(A, B),
    member(house(V,_,_,horse,_,_), Street),
    near(U, V),
    member(house(_,_,_,_,orangeJuice,luckyStrike), Street),
    member(house(_,japanese,_,_,_,parliaments), Street),
    member(house(T,_,blue,_,_,_), Street),
    near(1, T),
  	member(house(_,ZebraOwner,_,zebra,_,_), Street).


% Exercitiul 4

def(myTree, tree(1, tree(2, tree(4, nil, tree(7, nil, nil)), nil), tree(3, tree(5, nil, nil), tree(6, nil, nil)))).

% inordine
srd(nil, []).
srd(tree(Root, Left, Right), Result) :-
    srd(Left, ResultLeft),
    srd(Right, ResultRight),
    append(ResultLeft, [Root | ResultRight], Result).

% preordine
rsd(nil, []).
rsd(tree(Root, Left, Right), Result) :-
    rsd(Left, ResultLeft),
    rsd(Right, ResultRight),
    append([Root | ResultLeft], ResultRight, Result).

% postordine
sdr(nil, []).
sdr(tree(Root, Left, Right), Result) :-
    sdr(Left, ResultLeft),
    sdr(Right, ResultRight),
    append(ResultLeft, ResultRight, ResultWithoutRoot),
    append(ResultWithoutRoot, [Root], Result).

% testati scriind 
% def(myTree, Tree), srd(Tree, Result). 
% si schimbati srd cu rsd si sdr 

