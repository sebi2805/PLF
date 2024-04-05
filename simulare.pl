% Model - Test de laborator 
% 
% 
% Predicate predefinite in Prolog si breviar teoretic 
% 
% atom/1 - verifica daca un string este atom in Prolog
% atom = orice sir de caractere care incepe cu litera mica sau care e scris intre simboluri ''
% ?- atom(X). false
% ?- atom(myAtom). true 
% ?- atom('My Atom'). true 
%
% In scrierea regulilor, folosim structura
% Head :- Body. Daca Body e satisfacut, atunci satisfacem si Head. (Head if Body)
% Facts - contin doar Head, sunt cunostintele de baza
% Rules - cele care contin Body. In Body, putem inlantui cunostintele prin , (SI = conjunctie)
% 															respectiv prin ; (SAU = disjunctie)
% Pentru a limita recursia pe cazurile de baza, folosim !/0. (predicatul CUT)
% el este util cand avem clauze scrie una sub alta - atunci se face disjunctie intre clauze.
% Daca se satisface o clauza, Prolog incearca satisfacerea celorlalte clauze, imediat urmatoare.
% Pentru a opri acest mecanism, e util CUT. 
% Exemplu: base_case(params) :- !.  
%
% Operatorii aritmetici
% uzuali: +, -, *, / (floats), // (cat), div (cat), mod (rest), ** (exponentiere)
% egalitati: = (cauta unificator), == (structural), =:= (aritmetic) 
% is/2 - operator de atribuire aritmetica
% ?- X is 1 + 2. atunci X = 3
% is/2 functioneaza cand membrul drept este COMPLET instantiat 
% Y = 2, X is Y + 2. raspunde Y = 2, X = 4
% dar X is Y + 2. e eroare, pentru ca Y nu e instantiat ("Arguments are not sufficiently instantiated")
% 
% Listele in Prolog 
% [se, reprezinta, prin, paranteze, patrate, si, cu, elemente, separate, prin, virgula]
% pot contine elemente de orice tip, amestecate oricum [0, [], [X], f(X), Y] etc 
% notatia utila: [Head | Tail] (sau [H | T] in general, in probleme)
% Head-ul unei liste vide conduce la eroare!!! Tail-ul listei vide este lista vida
% [H | T] = [1] atunci H = 1, T = []
% [H | T] = [] atunci EROARE
% [A, B | T] = [a,b,c,d] atunci A = a, B = b, T = [c, d]
% [A, B | T] = [a, b] atunci A = a, B = b, T = []
% [A, B | T] = [a] atunci EROARE pentru ca B e in Head (inainte de | ) si nu se poate instantia
% length/2 - returneaza lungimea unei liste
% ?- length([1,2,3],L). atunci L = 3
% member/2 - verifica daca un element apare sau nu intr-o lista
% ?- member(b, [a,b,c,d]). atunci true
% ?- member(t, [a,b,c,d]). atunci false 
% union/3 - reuniunea a doua liste, cu rezultatul o lista reprezentand o multime - elementele sunt distincte
% ?- union([1,2,3,2],[1,4,3,2],L). atunci L = [1,4,3,2]
% append/3 - concateneaza doua liste, mai intai prima lista, apoi a doua 
% ?- append([1,2,3,2],[1,4,3,2],L). atunci L = [1, 2, 3, 2, 1, 4, 3, 2]
% string_chars/2 - split-uieste un sir de caractere intr-o lista de caractere corespunzatoare
% ?- string_chars(prolog, Res). atunci Res = [p, r, o, l, o, g]
% ?- numlist/3 - primeste doua capete de interval, si returneaza lista intregilor din intervalul inchis 
% ?- numlist(-2, 3, Res). atunci Res = [-2, -1, 0, 1, 2, 3]
% daca primul argument > al doilea, returneaza false 
% findall/3 - findall(X, P, L). pune in L toti acei X astfel incat sa respecte P 
% divisorsPairs(A, B, List) :- 
% 	numlist(A, B, Range),
% 	findall((X, Y), (member(X, Range), member(Y, Range), X mod Y =:= 0), List).
% i.e. gaseste toate perechile (X, Y)
% cu proprietatea ca X apartine Range, Y apartine Range si restul impartirii lui X la Y este 0
% 															(adica Y este divizor al lui X)
% si pune toate aceste perechi in List 
% 
%
% Predicate utile, dar care nu sunt definite in Prolog

% sum/2 - returneaza suma dintr-o lista
sum([], 0) :- !.
sum([H | T], Res) :-
    sum(T, ResT),
    Res is ResT + H.

% zip/3 - face perechi de elemente de pe aceeasi pozitie intre doua liste
% ?- zip([1,2,3],[a,b,c],Res). atunci Res = [(1, a), (2, b), (3, c)]
% zip se opreste la lungimea celei mai scurte dintre cele doua liste! 
% ?- zip([1,2,3,4],[a,b,c],Res). atunci Res = [(1, a), (2, b), (3, c)]
% ?- zip([1,2,3],[a,b,c,d,e],Res). atunci Res = [(1, a), (2, b), (3, c)]
zip([], _, []) :- !.
zip(_, [], []) :- !.
zip([H1 | T1], [H2 | T2], [(H1, H2) | TR]) :-
    zip(T1, T2, TR).

% all_symb/2 - verifica daca toate elementele dintr-o lista sunt egale cu un simbol dat
% ?- all_symb([a,a,a], a). atunci true
% ?- all_symb([a,A,a], a). atunci A = a, true (este satisfacut cand A = a)
all_symb([S], S) :- !.
all_symb([H | T], S) :-
    H = S,
    all_symb(T, S).

% max/2 - returneaza maximul dintr-o lista 
max([], 0) :- !.
max([H | T], MaxTail) :-
    max(T, MaxTail),
    MaxTail >= H, !.
max([H | T], H) :-
    max(T, MaxTail),
    H >= MaxTail.

% remove_duplicates/2 - returneaza lista primita ca prim argument, dar fara elemente duplicate
remove_duplicates([], []) :- !.
remove_duplicates([H | T], [H | TR]) :-
    not(elements_of(H, T)), 
    remove_duplicates(T, TR), !.
remove_duplicates([_ | T], TR) :-
    remove_duplicates(T, TR).

% Exemplu de BFS 
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

% Exemplu de arbori si parcurgere

def(myTree, tree(1, tree(2, tree(4, nil, tree(7, nil, nil)), nil), tree(3, tree(5, nil, nil), tree(6, nil, nil)))).

% inordine
srd(nil, []).
srd(tree(Root, Left, Right), Result) :-
    srd(Left, ResultLeft),
    srd(Right, ResultRight),
    append(ResultLeft, [Root | ResultRight], Result).

% Pentru UNIFICARE
% unify_with_occurs_check/2 verifica daca doi termeni pot unifica sau nu
% atentie la specificatia de limbaj! variabilele din limbaj sunt si cele din Prolog, deci se scriu cu Majuscula

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SFARSITUL BREVIARULUI TEORETIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exemple de subiecte
% ATENTIE! Structura testului final nu va fi ca in model, aici aveti doar exercitii de antrenament

% Exercitiul 1
% Fie un predicat employee_salary/2 care configureaza un nume de angajat si salariul acestuia.
% De exemplu, employee_salary(peter, 1000)
% Primind o lista de elemente employee_salary, determinati numele tuturor angajatilor care castiga o suma minima configurata.
% veti scrie un predicat employee_with_good_salary(InputList, MinimumSalary, ResultList).
% de exemplu
% ?- employee_with_good_salary([(peter, 1000), (oliver, 1200), (sam, 700), (ben, 15000), (sandra, 4500)], 1200, Res).
% Res = [oliver, ben, sandra]

% Exercitiul 2
% Primind o lista identica celei de la exercitiul 1, dar cu aparitii multiple ale unui angajat, 
% 	(considerand ca fiecare intrare reprezinta salariul intr-o anumita luna)
% 	determinati salariul mediu al fiecarui angajat. 
% de exemplu, daca peter apare (peter, 1000), respectiv (peter, 1200), in lista de rezultat va fi doar (peter, 1100)
% ?- employee_average_salary([(peter, 1000), (oliver, 1200), (sam, 700), (oliver, 800), (sam, 900), (sandra, 4500), (peter, 1200), (oliver, 400)], Result).
% Result = [(peter, 1100), (oliver, 800), (sam, 800), (sandra, 4500)]

% Exercitiul 3
% Primind o lista identica celei de la exercitiul 2, in lista de rezultat ordonati angajatii dupa salariul mediu castigat, in ordine descrescatoare. 
% ?- sort_employee_average_salary([(peter, 1000), (oliver, 1200), (sam, 700), (oliver, 800), (sam, 900), (sandra, 4500), (peter, 1200), (oliver, 400)], Result).
% Result = [(sandra, 4500), (peter, 1100), (oliver, 800), (sam, 800)]

% Exercitiul 4 - Dutch Royal Family 

mother(wilhelmina,juliana).
mother(juliana,beatrix).
mother(juliana,christina).
mother(juliana,irene).
mother(juliana,margriet).
mother(beatrix,friso).
mother(beatrix,alexander).
mother(beatrix,constantijn).
mother(emma,wilhelmina).
father(hendrik,juliana).
father(bernard,beatrix).
father(bernard,christina).
father(bernard,irene).
father(bernard,margriet).
father(claus,friso).
father(claus,alexander).
father(claus,constantijn).
father(willem,wilhelmina).

% citim mother(beatrix, alexander) ca "beatrix este mama lui alexander". aceeasi interpretare la father 

queen(beatrix).
queen(juliana).
queen(wilhelmina).
queen(emma).
king(willem).

% 4a) definiti urmatoarele predicate
% parent/2 - parent(X, Y). sa fie true cand X este parinte al lui Y 
% ruler(X) - sa fie true cand X este conducator 

% 4b) definiti un predicat predecessor/2, astfel incat predecessor(X, Y) sa ne spuna ierarhia de predecesori pentru un conducator X
% (vrem sa raspundem, de fapt, la cine precede pe cine)

% Exercitiul 5 
% Fie L = (R, F, C, ari) o structura de ordinul I, unde F = {f, g, h}, C = {u, v, w} iar ari(f) = 2, ari(g) = 1, ari(h) = 3
% Considerand a, b, c, d variabile, care dintre urmatorii termeni se pot unifica?
% a) f(g(a), h(b, c, v)) cu f(g(u), h(g(w), w, g(a)))
% b) f(g(a), h(b, c, d)) cu f(g(u), h(g(w), w, g(a)))

% Exercitiul 6 
% Fie predicatele succ/2 si obj/1 (pentru succesor si obiectiv din BFS, cu alta denumire pentru ca le avem deja in fisier cu s/2 si objective/1). 
% Adaptati algoritmul BFS astfel incat sa pastram drumurile cele mai scurte care ajung de la un nod de start la un nod obiectiv. 

% Exercitiul 7
% verificati daca un sir de caractere primit ca intrare, si reprezentat ca un atom, este palindrom sa nu
% palindrome(prolog). atunci false
% palindrome(ele). atunci true 

% Exercitiul 8
% Fie urmatoarea baza de cunostinte, definita de predicatele 
% 	employee_info(name, department_number, scale)
%	department(department_number, department_name)
%	salary(scale, amount)

employee_info(mcardon,1,5).
employee_info(treeman,2,3).
employee_info(chapman,1,2).
employee_info(claessen,4,1).
employee_info(petersen,5,8).
employee_info(cohn,1,7).
employee_info(duffy,1,9).
department(1,board).
department(2,human_resources).
department(3,production).
department(4,technical_services).
department(5,administration).
salary(1,1000).
salary(2,1500).
salary(3,2000).
salary(4,2500).
salary(5,3000).
salary(6,3500).
salary(7,4000).
salary(8,4500).
salary(9,5000).

% a) determinati toti angajatii din departamentul 1 si care au scale > 2 
% Exemplu pentru rezolvare
% ?- employee_info(Name, Department_Number, Scale), Department_Number = 1, Scale > 2 
% acum, puneti toate aceste rezultate intr-o singura lista 

% b) determinati toti angajatii dintr-un anume departament 

% c) selectati name si scale al angajatilor din departamentul 1, si scale > 3 

