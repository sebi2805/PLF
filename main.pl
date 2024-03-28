father(bob, alice).
father(bob, carol).
father(bruce, eva).

mother(alice, david).
mother(carol, eva). 
mother(carol, frank).

parent(F, M, C) :-
    father(F, C);
    mother(M, C).

brother(B1, B2) :-
    parent(F1, M1, B1),
    parent(F1, M1, B2).

grandfather(GF1, GM1, GF2, GM2, C) :- !.



inversa(L, R) :-
    inversa_aux(L, [], R).

inversa_aux([], Acc, Acc).    
inversa_aux([H | T], Acc, R):-
    inversa_aux(T, [H | Acc], R).