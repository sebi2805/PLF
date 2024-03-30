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