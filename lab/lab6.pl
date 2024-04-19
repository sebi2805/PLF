% Exercitiul 6 
% Fie predicatele succ/2 si obj/1 (pentru succesor si obiectiv din BFS, cu alta denumire pentru ca le avem deja in fisier cu s/2 si objective/1). 
% Adaptati algoritmul BFS astfel incat sa pastram drumurile cele mai scurte care ajung de la un nod de start la un nod obiectiv. 

s(1, 2).
s(1, 3).
s(2, 4).
s(2, 3).
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

min_length([H], H).
min_length([H|T], H):-
    length(H, Lnew),
    min_length(T, MinList),
    length(MinList, Lold),
    Lnew<Lold,
    !.
% si daca e egal pune la fel si faci array
% daca lungimea e mai mare atunci inlocuieste arrayul

min_length([_|T], R):-
    min_length(T, R).     
solve_2(Start, Solution):-
    solve(Start, Solutions), 
    min_length(Solutions, Solution).


% Exercitiul 7
% verificati daca un sir de caractere primit ca intrare, si reprezentat ca un atom, este palindrom sa nu
% palindrome(prolog). atunci false
% palindrome(ele). atunci true 


check_equal([H1|T1], [H1|T2]):-
    check_equal(T1, T2).
palindrome(Word) :-
    string_chars(Word, L), 
    reverse(L, L). 


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

employees(Dep, Scale, S):-
    findall(X, (employee_info(X, Dep, Scale1), Scale1 > Scale), S).


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

%  unify_with_occurs_check(f(g(A), h(B, C, d)), f(g(u), h(g(w), w, g(A)))). 
% si pui in comentariu unificarea 