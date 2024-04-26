% tinyurl.com/plf-lab-02-2024
% Ex 1: Sa se implementeze un predicat fib/2 care primeste ca prim argument un nr natural n si intoarce in al 2-lea argument
% al n-lea termen al sirului lui Fibonacci,

% Fn=Fn-2 + Fn-1

% fib(0, 1).
% fib(1,1).
% fib(N,F):- N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2.

%  Solutie proasta - trebuie sa calculez de multe ori acelasi lucru si arborele ocupa mult spatiu 

% facem iterativ
% Obs: pot face overload pe functii 
% Solutia 2:
% F1, F0 - ultimele 2 numere din sir
% I - indexul curent
% N - indexul la care ma opresc
% F - rezultatul 
fib(F,_,N,N,F). %Cazul de oprire- F e rezultatul final ( ultimul generat) si penultimul nu ma intereseaza
fib(F1, F0, I, N, F):- F2 is F1 + F0,
                       NextI is I + 1,
                       fib(F2,F1, NextI,N,F).
% Pot sa o modific sa ii dau 2 argumente doar
fib(0,1).
fib(1,1). % base case
fib(N, F):- fib(1,1,1,N,F).

% Ex2: Sa se scrie un predicat list_length/2 care primeste ca prim argument o lista si returneaza in al doilea argument, lungimea acestuia

list_length([],0).
list_length([_|T],R):- list_length(T,R1), R is R1 +1. 

% Ex3: Scrieti un predicat list_sum/2 care sa calculeze suma elementelor din lista primita ca prim argument.

list_sum([],0).
list_sum([H|T],Rez):- list_sum(T,Rez2), Rez is Rez2 + H.

% Ex4: Scrieti un predicat elements_of/2 care verifica daca primul argument este element al listei din cel de-al doilea argument.

elements_of(X,[X|_]).
elements_of(Y,[_|T]):-elements_of(Y,T).

% Ex5: Definiti un predicat all_a/1 care primeste ca argument o lista si care verifica daca argumentul sau este format doar din a-uri.
% Scrieti si o forma generala care sa verifice ca toate elementele sunt egale cu un simbol dat.
% Ce tip de egalitate utilizati?

% all_a([]). se poate si doar lista vida caz de baza
all_a([a]).
all_a([a|T]):- all_a(T).
% sau: all_a([H|T]):- H = a, all_a(T).

% Folosim egalitatea de substitutie 

% Ex 6:
% Scrieti un predicat trans_a_b/2 care translateaza o lista de a-uri intr-o lista de b-uri.
% Predicatul trebuie sa fie adevarata daca primul argument este o lista de a-uri iar al doilea este o lista de b-uri de aceeasi lungime.

trans_a_b([a],[b]).
trans_a_b([a|Ta],[b|Tb]):- trans_a_b(Ta,Tb).