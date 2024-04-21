% Predicatul numlist/3 prime ̧ste dou ̆a capete de interval A  ̧si B  ̧si returneaz ̆a ˆın al treilea argument
% toate elementele ˆıntregi din intervalul [A,B], Dac ̆a A>B, atunci ˆıntoarce false.
% ?- numlist(-2, 3, R).
% R = [-2, -1, 0, 1, 2, 3]
% Predicatul findall/3, descris de findall(X, P, R) g ̆ase ̧ste tot ̧i acei X care respect ̆a proprietatea
% P,  ̧si ˆıi returneaz ̆a ˆın lista R. Predicatul setof are acela ̧si comportament, dar asigur ̆a c ̆a rezultatul
% este strict o mult ̧ime (nu cont ̧ine elemente duplicate).

% 1. Scriet ̧i un predicat divisorsPairs/3 care determina toate perechile (X, Y), astfel ˆıncˆat X  ̧si Y
% sunt elemente ale listei determinata de primele dou ̆a argumente ale predicatului, iar al treilea
% argument este lista perechilor cerute.
% ?- divisorsPairs(2, 6, LR).
% LR = [(2,2), (3,3), (4,2), (4,4), (5,5), (6,2), (6,3), (6,6)]

divisorsPairs(A, B, List) :- numlist(A, B, Range),
                             findall((X,Y), (member(X, Range), member(Y, Range), X mod Y =:= 0), List).

% Scrieti un predicat oddIndexes/2 care sa extraga toate elementele de pe pozitiile impare dintr-o lista, fara a utiliza recursia

oddIndexes(List, Result) :- findall(X, (nth0(Index, List, X), 1 is Index mod 2), Result).

% sau folosind zip

zip([], _, []):- !.
zip(_, [], []):- !.
zip([H1|T1], [H2|T2], [(H1, H2)|Result]):- zip(T1, T2, Result).

oddIndexes2(List, Result):- length(List, Length),
                            numlist(0, Length, Indexes),
                            zip(List, Indexes, Zipped),
                            findall(X, (member((X, Index), Zipped), 1 is Index mod 2), Result).


% Exercitiul 2 - BFS

s(1, 2).
s(1, 3).
s(2, 4).
s(2, 5).
s(3, 5).
s(3, 4).
objective(5).

% Return de tipul [5, 2, 1]

extend([Node|Path], Result):- findall([NewNode, Node|Path],
                                      (s(Node, NewNode), not(member(NewNode, [Node|Path]))),
                                      Result),!. % ! ca sa nu mai caute dupa ce a gasit o solutie
extend(_, []). % ca sa nu dea false daca nu gaseste nicio extindere

breadthfirst([[Node|Path]|_], [Node|Path]):- objective(Node).
breadthfirst([Path|TailPath], Solution):- extend(Path, ExtendedPath),
                                        append(TailPath, ExtendedPath, NewPath),
                                        breadthfirst(NewPath, Solution).

solve(Start, Solution):- findall(S, breadthfirst([[Start]], S), Solution).

% Exercitiul 3
% Vrem sa rezolvam in Prolog urmatorul puzzle: Zebra_Puzzle
% Pentru fiecare personaj stim urmatoarele:

% Locuieste intr-o casa care are o anumita culoare;
% Are o nationalitate;
% Are un anumit animal de companie;
% Are o bautura preferata;
% Fumeaza un anumit tip de tigara.

% Avem urmatoarele informatii:
% 1. Sunt 5 case.
% 2. Englezul locuieste in casa rosie.
% 3. Spaniolul are un caine.
% 4. In casa verde se bea cafea.
% 5. Ucraineanul bea ceai.
% 6. Casa verde este imediat in dreapta casei bej.
% 7. Fumatorul de Old Gold are un melc.
% 8. In casa galbena se fumeaza Kools.
% 9. In casa din mijloc se bea lapte.
% 10. Norvegianul locuieste in prima casa.
% 11. Fumatorul de Chesterfield locuieste langa cel care are o vulpe.
% 12. Kools sunt fumate in casa de langa cea care se tine calul.
% 13. Fumatorul de Lucky Strike bea suc de portocale.
% 14. Japonezul fumeaza Parliament.
% 15. Norvegianul locuieste langa casa albastra.

% Scopul este sa determinam nationalitatea posesorului zebrei. Pentru a putea rezolva, veti defini urmatoarele:

% 1. Definiti un predicat right(X,Y) care este adevarat cand X is Y + 1.
% 2. Definiti un predicat left(X,Y) care este adevarat cand Y is X + 1.
% 3. Definiti un predicat near(X,Y) care este adevarat cand X este sau la stanga, sau la dreapta lui Y.

% Casele se reprezinta prin:

right(X,Y) :- X is Y + 1.
left(X,Y) :- Y is X + 1.
near(X,Y) :- right(X,Y) | left(X,Y).

house(Number,Nationality,Colour,Pet,Drink,Cigarettes).
solution(Street, ZebraOwner) :-
    Street = [
house(1,_,_,_,_,_,_),
house(2,_,_,_,_,_,_),
house(3,_,_,_,_,_,_),
house(4,_,_,_,_,_,_),
house(5,_,_,_,_,_,_)
],

member(house(_,english,red,_,_,_), Street), % Englezul locuieste in casa rosie.
member(house(_,spanish,_,dog,_,_), Street), % Spaniolul are un caine.
member(house(_,_,green,_,coffee,_), Street), % In casa verde se bea cafea.
member(house(_,ukrainian,_,_,tea,_), Street), % Ucraineanul bea ceai.
right(X,Y), member(house(X,_,green,_,_,_), Street), member(house(Y,_,beige,_,_,_), Street), % Casa verde este imediat in dreapta casei bej.
member(house(_,_,_,snails,_,old_gold), Street), % Fumatorul de Old Gold are un melc.
member(house(_,_,yellow,_,_,kools), Street), % In casa galbena se fumeaza Kools.
member(house(3,_,_,_,milk,_), Street), % In casa din mijloc se bea lapte.
member(house(1,norwegian,_,_,_,_), Street), % Norvegianul locuieste in prima casa.
near(X,Y), member(house(X,_,_,_,_,chesterfield), Street), member(house(Y,_,_,fox,_,_), Street), % Fumatorul de Chesterfield locuieste langa cel care are o vulpe.
near(X,Y), member(house(X,_,_,_,_,kools), Street), member(house(Y,_,_,horse,_,_), Street), % Kools sunt fumate in casa de langa cea care se tine calul.
member(house(_,_,_,_,orange_juice,lucky_strike), Street), % Fumatorul de Lucky Strike bea suc de portocale.
member(house(_,japanese,_,_,_,parliament), Street), % Japonezul fumeaza Parliament.
near(X,Y), member(house(X,norwegian,_,_,_,_), Street), member(house(Y,_,blue,_,_,_), Street), % Norvegianul locuieste langa casa albastra.
member(house(_,ZebraOwner,_,zebra,_,_), Street). % Scopul este sa determinam nationalitatea posesorului zebrei.

% Nu merge - dar Japonezul fumeaza Parliament, deci Japonezul are zebra

% Exercitiul 4

% Consideram predicatele tree/3 pentru reprezentarea arborilor binari, respectiv nil/0
% pentru reprezentarea arborelui vid. De exemplu, tree(Anything, nil, nil) este o frunza. Implementati:

% Cele trei tipuri de parcurgeri ale arborilor binari(in-ordine,pre-ordine, post-ordine)
% Scrieti un predicat care sa determine toate frunzele din arbore.

% in-ordine
srd(nil, []).
srd(tree(R, nil, nil), [R]).
srd(tree(R, TL, TR), Result):- srd(TL, ResultL),
                              srd(TR, ResultR),
                              append(ResultL, [R|ResultR], Result).

% pre-ordine
rds(nil, []).
rds(tree(R, nil, nil), [R]).
rds(tree(R, TL, TR), Result):- rds(TL, ResultL),
                              rds(TR, ResultR),
                              append([R|ResultL], ResultR, Result).

% post-ordine
drs(nil, []).
drs(tree(R, nil, nil), [R]).
drs(tree(R, TL, TR), Result):- drs(TL, ResultL),
                              drs(TR, ResultR),
                              append(ResultL, ResultR, Temp),
                              append(Temp, [R], Result).

% frunze
tree(nil, []).
tree(tree(R, nil, nil), [R]).
tree(tree(_, TL, TR), Result):- tree(TL, ResultL),
                              tree(TR, ResultR),
                              append(ResultL, ResultR, Result).

% Example for tree
% tree(1, tree(2, tree(4, nil, nil), tree(5, nil, nil)), tree(3, tree(6, nil, nil), tree(7, nil, nil))).





