% algoritmul de unificare 
% necesita un algoritm care verifica daca exista o substitutie care 
% dupa aplicarea ei doi termeni devin identici
% Logica de ordinul 1 vorbim de un limbaj care are la baza simboluri de functii, constante, aritate si relatii
% L=(F, R, C, ari) ari: F reunit cu R -> N 
% multimea functiilor, a relatiilor, a constantelor
% variabile sunt la fel ca in calculul propozitional
% multimea de simboluri este la fel = Var reunit  { ->, !, (,), = , oricare/V} plus cele 2
% Expr = {lambda} reunit cu Sim reunit cu Sim^n din termeni putem construi formule atomice si din ele formule 
% notam cu Fm multimea tuturor functiilor din F cu proprietate cu ari(f)=m 
% multimea de termeni este reprezentata de: orice variabila este un termen, orice constanta este un termen
% daca t1, .. tn sunt deja termeni si f este o functie cu ari(f)=n atunci f(t1, .. tn) este un termen

% o substitutie este o functie care asociaza fiecarei variabile un termen
% o substitutie este o functie de la Var in Term

% Scop in unificare avem t1 si t2 termeni si vrem sa gasim o substitutie care sa faca t1 si t2 identici
% rezolvam prin algoritm
% foloseste 2 multimi multimii de substitutii si o multime de rezolvat si operatei


lovable :- ['L', 'O', 'V', 'A', 'B', 'L', 'E'].
energy :- ['E', 'N', 'E', 'R', 'G', 'Y'].
savage :- ['S', 'A', 'V', 'A', 'G', 'E'].
whiskers :- ['W', 'H', 'I'].
agile :- ['A', 'G', 'I', 'L', 'E'].
purring: ['P', 'U', 'R', 'R', 'I', 'N', 'G'].

word(lovable).
word(energy).
word(savage).
word(whiskers).
word(agile).
word(purring).


list_pos_aux([H|_], I, I, H).
list_pos_aux([_|T], I, C, E) :- C1 is C + 1, list_pos_aux(T, I, C1, E).


list_pos(L, I, E) :-
    list_pos_aux(L, I, 1, E).


solution(W1, W2, W4, W5, W5, W6, L) :- 
            word(W1),
            string_chars(W1).


connected(1,2).
connected(3,4).
connected(5,6).
connected(7,8).
connected(9,10).
connected(12,13).
connected(13,14).
connected(15,16).
connected(17,18).
connected(19,20).
connected(4,1).
connected(6,3).
connected(4,7).
connected(6,11).
connected(14,9).
connected(11,15).
connected(16,12).
connected(14,17).
connected(16,19).


path(A, B) :-
    connected(A, B).

path(A, B) :-
    connected(A, C),
    path(C, B).    