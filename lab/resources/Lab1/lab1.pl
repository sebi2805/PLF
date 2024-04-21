male(sam).
male(oliver).
male(ben).
male(peter).
male(roger).
female(elizabeth).
female(sandra).
female(mary).
female(lisa).
female(olivia).
parent_of(sandra, sam).
parent_of(sandra, elizabeth).
parent_of(ben, sam).
parent_of(ben, elizabeth).
parent_of(peter, oliver).
parent_of(peter, sandra).
parent_of(roger, ben).
parent_of(roger, lisa).
parent_of(olivia, roger).
parent_of(olivia, mary).

% EXERCITIUL 1
% Definiti, pentru baza de cunostiinte de mai sus, predicatele urmatoare: 
% mother_of/2, brother_of/2, sister_of/2, uncle_of/2, aunt_of/2, grandfather_of/2, grandmother_of/2. 
% Verificati aceste predicate punand intrebari si urmarind pe arborele genealogic de mai sus.
% Tinand cont ca parent_of(X,Y) se citeste : Y este parintele lui X

% mother_of(X, Y) :- parent_of(X, Y),
%                    female(Y).

% brother_of(X, Y) :- parent_of(X, Z),
%                     parent_of(Y, Z),
%                     male(Y),
%                     X \== Y.

% sister_of(X, Y) :- parent_of(X, Z),
%                    parent_of(Y, Z),
%                    female(Y),
%                    X \== Y.

% uncle_of(X, Y) :- parent_of(X, Z),
%                   brother_of(Z, Y).

% grandfather_of(X, Y) :- parent_of(X, Z),
%                         parent_of(Z, Y),
%                         male(Y).

% grandmother_of(X, Y) :- parent_of(X, Z),
%                         parent_of(Z, Y),
%                         female(Y).

% Remake for better understanding:
% Y is mother of X
mother_of(X,Y):- parent_of(X,Y), female(Y).

% Y is father of X
father_of(X,Y):- parent_of(X,Y), male(Y).

% Y is brother of X
brother_of(X,Y):- parent_of(X,Z), parent_of(Y,Z), male(Y), X\==Y.

% Y is sister of X
sister_of(X,Y):- parent_of(X,Z), parent_of(Y,Z), female(Y), X\==Y.

% Y is uncle of X

% mama   tata - sora/frate(pentru mine este matusa/unchi)
%   \    /
%     eu  
% Z= tata, trebuie sa fie parinte al meu (X)
uncle_of(X,Y):- parent_of(X,Z), brother_of(Z,Y).

% Y is aunt of X
uncle_of(X,Y):- parent_of(X,Z), sister_of(Z,Y).

% Y is grandfather of X 
grandfather_of(X,Y):- parent_of(X,Z), father_of(Z,Y).

% Y is grandmother of X
grandmother_of(X,Y):- parent_of(X,Z), mother_of(Z,Y).

% EXERCITIUL 2
% Utilizand aceeasi baza de cunostiinte de mai sus, definiti un predicat ancestor_of/2, 
% care sa verifice daca al doilea argument este stramos al primului.
% Predicatul trebuie sa fie true pentru toate intrebarile pe linia directa, de exemplu: ?- ancestor_of(olivia,sam).

% cautare recursiva a stramosului
ancestor_of(X,Y) :- parent_of(X,Y). % cazul de baza
ancestor_of(X,Y) :- parent_of(X,Z), ancestor_of(Z,Y). % cazul recursiv

% Remake:
% ancestor_of(X,Y):- parent_of(X,Y).
% ancestor_of(X,Y):- parent_of(X,Z), ancestor_of(Z,Y).


% EXERCITIUL 3
% Definiti un predicat distance/3 pentru a calcula distanta dintre doua puncte intr-un plan 2-dimensional.

% Remake:
% distance((X1,Y1),(X2,Y2),Res):- Res is sqrt((X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1)).
 
distance((X1, Y1), (X2, Y2), R) :- R is sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)).

% EXERCITIUL 4
% Fie write/1 un predicat care scrie argumentul primit (un atom) la STDOUT.
% Folositi acest predicat pentru a defini un predicat nou, write n/2 care primeste ca argumente un numar natural nenul si
% un atom si afiseaza la STDUT un triunghi format din atomul primit si din dimensiunea primita.
% De exemplu, pentru write n(5, *) se va afisa:

% *****
% ****
% ***
% **
% *
% Modificati afisarea pentru a obtine reprezentarea:
% *
% **
% ***
% ****
% *****

write_line(_, 0) :- nl. % cazul de baza -nl = new line
write_line(S, N) :- write(S), N1 is N - 1, write_line(S, N1). % cazul recursiv, write(S) - scrie simbolul

draw_piramid(0, _):- nl.
draw_piramid(N, Symbol):- write_line(Symbol, N), N1 is N-1, draw_piramid(N1, Symbol). % Afiseaza primul caz de piramida descrescator

draw_aux(N, _, N):-nl.
draw_aux(N, Symbol, Start):- write_line(Symbol, Start), NewStart is Start+1, draw_aux(N,Symbol,NewStart).

draw_piramid_2(N, Symbol):- draw_aux(N,Symbol,0).

% EXERCITIUL 5
% Scrieti un predicat min/3 care sa calculeze minimul a doua elemente. Ce observati in implementare ?
my_min(X,Y,Res):- X =< Y, Res is X;
                  Y =< X, Res is Y.

my_min_2(X,Y,Res):- X =< Y -> Res is X; Res is Y.

% EXERCIȚIUL 6 
% Scriet,i un program Prolog care, primind trei puncte ˆın plan, verific ̆a dac ̆a ele determin ̆a un triunghi
% dreptunghic. Punctele sunt reprezentate ca perechi (X, Y).

