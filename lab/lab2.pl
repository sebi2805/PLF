% un predicat fib/2
% care primeste ca prim argument un numar natural N si intoarce al N-lea numar din sirul lui Fibonacci


fib(0, 1).
fib(1, 1).


fib(N, F) :-
    fib_aux(N, 0, 1, F).
 


fib_aux(0, _, Acc, Acc). % ??
fib_aux(1, _, Acc, Acc ).

fib_aux(N, Acc1, Acc2, F).
    N > 1,
    N1 is N - 1, 
    NewAcc is Acc1 + Acc2,  
    fib_acc(N1, Acc2, NewAcc, F).