/**
          (*) Find the last element of a list.
          Example:
          ?- my_last(X,[a,b,c,d]).
          X = d
**/
p1(X, [X]).
p1(X, [_ | T ]) :- 
          my_last(X , T).

/**
          (*) Find the last but one element of a list.
**/
p2(X, [X, _]).
p2(X, [_ | T]) :- 
          p2(X, T).

/**
           Find the K'th element of a list.
          The first element in the list is number 1.
          Example:
          ?- element_at(X,[a,b,c,d,e],3).
          X = c
**/
p3(X, [X | _], 1).
p3(X, [_ | T], I) :-
          I1 is I - 1,
          p3(X, T, I1).

/**
          Find the number of elements of a list.
**/

p4([], 0).
p4([_| T], R) :-
          p4(T, R1),
          R is R1 + 1.

/**
          Reverse a list.
**/

p5_aux([], L, L).
p5_aux([H1 | T1], L, Acc) :-
          p5_aux(T1, L, [H1|Acc]).

p5(L1, L2) :-
          p5_aux(L1, L2, []).

/**
          Find out whether a list is a palindrome.
          A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
**/

% if anyone reads this code im sorry bcs of my bad naming im using p5 as reverse
p6(L):-
          p5(L, L).

/**
          (**) Flatten a nested list structure.
          Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

          Example:
          ?- my_flatten([a, [b, [c, d], e]], X).
          X = [a, b, c, d, e]

          Hint: Use the predefined predicates is_list/1 and append/3
**/

p7([], []).
p7([H | T], R) :-
          is_list(H),
          % so if we have a list in a list we need to also flatten it
          p7(H, R2),
          p7(T, R1),
          append(R2, R1, R).
p7([H | T], [H| R1]) :-
          p7(T, R1).

/**
          (**) Eliminate consecutive duplicates of list elements.
          If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

          Example:
          ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
          X = [a,b,c,a,d,e]
**/

p8([], []).
p8([H, H | T], R) :-
       p8([H|T], R).
p8([H1, H2 | T], [H1 | R]) :-
          p8([H2 | T], R).

/**
(**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:
?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
**/

p9([], []).
% TODO 

% P09 (**):  Pack consecutive duplicates of list elements into sublists.

% pack(L1,L2) :- the list L2 is obtained from the list L1 by packing
%    repeated occurrences of elements into separate sublists.
%    (list,list) (+,?)

pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

% transfer(X,Xs,Ys,Z) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed and transfered to Z

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).



/**
(*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.

Example:
?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
**/

p10_aux([], []).
p10_aux([[LPHH | LPHT]|LPT], [[Count|LPHH] | R]):- 
       length([LPHH | LPHT], Count),
       p10_aux(LPT, R).
p10(L, R) :-
       pack(L, LP),
       p10_aux(LP, R).

/**
(*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.

Example:
?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[4,a],b,[2,c],[2,a],d,[4,e]]
**/
p11_aux([], []).
p11_aux([[LPHH]|LPT], [LPHH, R]) :-
       p11_aux(LPT, R), !.
p11_aux([[LPHH | LPHT]|LPT], [[Count, LPHH] | R]):- 
       length([LPHH | LPHT], Count),
       p11_aux(LPT, R).  
p11(L, R):-
       pack(L, LP),
       p11_aux(LP, R).

/**
(**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
**/
p12_aux([0 , A], [A]).
p12_aux([C , A], [A|R]) :-
       C1 is C-1,
       p12([C1, A], R).
p12_aux(A, [A]).

p12([],[]). 
p12([H|T], R):-
       p12_aux(H, R1),
       p12(T, R2),
       append(R1, R2, R).

decode([], []).
decode([[1, E]|T], [E|R]) :- decode(T, R).
decode([[N, E]|T], [E|R]) :- 
    N > 1, 
    N1 is N - 1, 
    decode([[N1, E]|T], R).
decode([E|T], [E|R]) :- \+ is_list(E), decode(T, R).

/**
(*) Duplicate the elements of a list.
Example:
?- dupli([a,b,c,c,d],X).
X = [a,a,b,b,c,c,c,c,d,d]
**/
p14([], []).
p14([H|T], [H,H|R]):-
          p14(T, R).

/**
(**) Duplicate the elements of a list a given number of times.
Example:
?- dupli([a,b,c],3,X).
X = [a,a,a,b,b,b,c,c,c]

What are the results of the goal:
?- dupli(X,3,Y).
**/
p15_aux(_, 0, []).
p15_aux(E, C, [E|R]):-
       C1 is C-1,
       p15_aux(E, C1, R).

p15([], _, []).
p15([H|T], C, R) :-
       p15_aux(H, C, R1), 
       p15(T, C, R2), 
       append(R1, R2, R).

/**
(**) Drop every N'th element from a list.
Example:
?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
X = [a,b,d,e,g,h,k]
**/

p16_aux([], _, _, []).
p16_aux([_|T], N, N, R) :-
       !,
       p16_aux(T, N, 0, R).
p16_aux([H|T], N, I, [H|R]) :-
       INew is I+1, 
       p16_aux(T, N, INew, R).
p16(L, N, R) :- p16_aux(L, N, 0, R).


/**
(*) Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

Example:
?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
L1 = [a,b,c]
L2 = [d,e,f,g,h,i,k]
**/
p17_aux([], _, _, [], []).
p17_aux(L, N, N, [], L):- !.
p17_aux([H|T], I, N, [H|R1], R2):-
       INew is I+1,
       p17_aux(T, INew, N, R1, R2).

p17(L, N, R1, R2) :-
       p17_aux(L, 0, N, R1, R2).

/**
       (**) Extract a slice from a list.
       Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

       Example:
       ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
       X = [c,d,e,f,g]
**/

p18([H|_], 1, 1, [H]):-!.
p18([H|T], 1, End, [H|R]):-
       End1 is End-1,
       p18(T, 1, End1, R),
       !.

p18([_|T], Start, End, R):-
       Start1 is Start-1,
       End1 is End-1,
       p18(T, Start1, End1, R).
