aexp(I) :- integer(I). 
aexp(X) :- atom(X).  
aexp(A1 * A2) :- aexp(A1), aexp(A2).
aexp(A1 + A2) :- aexp(A1), aexp(A2).
aexp(A1 - A2) :- aexp(A1), aexp(A2).
aexp([]):-!.
aexp([A1 | A2]) :- aexp(A1), aexp(A2).