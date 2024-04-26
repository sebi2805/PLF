% Algoritmul de unificare !!!! 
%  Il folosim in logica de ordinul I ca sa vedem daca 2 termeni sunt egali 
% (Daca exista o substitutie astfel incat dupa aplicarea ei, doi termeni pot fi considerati ca fiind egali)

%  Logica de ord I : L=(F,R,C,ari); ari:F U R -> N* , F=functii, R=relatii, C=constante, ari=aritatea functiilor si relatiilor
% Variabilele sunt la fel ca in calculul propozitional 
% Sim= Var U {->,(,), =, oricare, non}
% Expr= {lambda} U Sim U U(uniunea de la n>= 2 din Sim)

% Termeni -> Formule atomice -> Formule (-> inseamna "pot caonstrui")
% Multimea de termeni se noteaza cu Trm indice L
% Multimea de formule se noteaza cu Form indice L

%  Fm = {f apartine F| ari(f)=m} ( functii cu aceeasi aritate) Fn - functie cu aritate n

% Trm indice L este caracterizata de: - orice variabila este un termen ( v apartine Var)
%                                      - orice constanta este un termen (c apartine C)
%                                      - daca f apartine Fn si t1, t2, ..., tn sunt termeni, atunci f(t1, t2, ..., tn) este un termen


% O substitutie teta: Var -> Trm indice L ; In general notam [u/x]t ( u il inlocuieste pe x in t), u si t sunt termeni, si x e variabila 

% Ex: [a/x]h(x,y) = h(a,y)

% Scop in unificare: Avem t1, t2 termeni ( apartin Trm indice L) si vrem sa vedem daca exista o substitutie teta(teta:Var->Trm indice L) astfel incat teta(t1)=teta(t2)

% Rezolvam algoritmic:

% ex: h(x,a)=h(b,y);
%  Multimea solitie                Multimea de rezolvat                    Operatie 
        %  S                           R, t=t                              Scoate
        %  S                              R
% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%          S                          R, f(t1,..tn)=f(t1',..tn')               Descopmunere
%          S                          R, t1=t1',.. tn=tn'                               
% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%          S                          R, x=t sau t=n                        Rezolva
%       x=t,[t/x] S                        [t/x]R
% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Cazuri de esec: 
% 1. f(...) = g(...) (f si g sunt functii diferite)
% Obs: constantele sunt functii de aritate 0, deci nici f=g nu se poate
% 2. x=t sau t=x si x apare in variabilele lui t
% ex: x = h(x) sau h(x) = x sunt esec

word(lovable).
word(energy).
word(savage).
word(whiskers).
word(agile).
word(purring).

list_pos([H | _], Index, Index, H) :- !.
list_pos([_ | T], CurrentIndex, Index, Result) :-
    NewIndex is CurrentIndex + 1,
    list_pos(T, NewIndex, Index, Result).

list_pos(List, Index, Result) :-
    list_pos(List, 0, Index, Result).

solution(W1, W2, W3, W4, W5, W6, L) :-
    word(W1), word(W2), word(W3), word(W4),
    word(W5), word(W6),
    W1 \== W2, W1 \== W3, W1 \== W4, W1 \== W5, W1 \== W6,
    W2 \== W3, W2 \== W4, W2 \== W5, W2 \== W6,
    W3 \== W4, W3 \== W5, W3 \== W6,
    W4 \== W5, W4 \== W6,
    W5 \== W6,
    string_chars(W1, L1),
    string_chars(W2, L2),
    string_chars(W3, L3),
    string_chars(W4, L4),
    string_chars(W5, L5),
    string_chars(W6, L6),
    list_pos(L1, 4, Int1),
    list_pos(L6, 0, Int1),
    list_pos(L2, 5, Int2),
    list_pos(L6, 1, Int2),
    list_pos(L3, 6, Int3),
    list_pos(L6, 2, Int3),
    list_pos(L4, 6, Int4),
    list_pos(L6, 3, Int4),
    list_pos(L5, 4, Int5),
    list_pos(L6, 4, Int5),
    list_pos(L1, 1, S1),
    list_pos(L2, 2, S2),
    list_pos(L3, 3, S3),
    list_pos(L4, 3, S4),
    list_pos(L5, 1, S5),
    L = [S1, S2, S3, S4, S5].

% member(X, L) -> X este membru in lista L
% de folosit la exercitiul 2 

% Scriet ̧i un predicat path/2 care indic ̆a dac ̆a dintr-un punct putet ̧i s ̆a ajunget ̧i ˆıntr-un alt
% punct (ˆın mai mult ̧i pa ̧si), legˆand conexiunile din baza de cuno ̧stint ̧e. Drumurile se consider ̆a
% unidirect ̧ionale (relat ̧ia nu este simetric ̆a).

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

