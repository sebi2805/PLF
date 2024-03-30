% Predicatul principal care inițiază adunarea, cu gestionarea transportului (carry)
adun(N, M, R) :-
    reverse(N, NRev),
    reverse(M, MRev),
    adun_cifre(NRev, MRev, 0, RRev),
    reverse(RRev, R).

% Cazul de bază: când ambele liste sunt goale și nu există transport, rezultatul este o listă goală.
adun_cifre([], [], 0, []) :- !.

% Dacă ambele liste sunt goale dar există un transport, acesta trebuie adăugat la rezultat.
adun_cifre([], [], Carry, [Carry]) :- Carry > 0.

% Dacă una dintre liste este goală, pur și simplu "transferăm" cifrele și transportul în rezultat.
adun_cifre([], [M|Ms], Carry, [R|Rs]) :-
    !, Sum is M + Carry, NextCarry is Sum div 10, R is Sum mod 10,
    adun_cifre([], Ms, NextCarry, Rs).

adun_cifre([N|Ns], [], Carry, [R|Rs]) :-
    !, Sum is N + Carry, NextCarry is Sum div 10, R is Sum mod 10,
    adun_cifre(Ns, [], NextCarry, Rs).

% Cazul general: adunăm cifrele și transportul, apoi continuăm recursiv.
adun_cifre([N|Ns], [M|Ms], Carry, [R|Rs]) :-
    Sum is N + M + Carry,
    NextCarry is Sum div 10,
    R is Sum mod 10,
    adun_cifre(Ns, Ms, NextCarry, Rs).
% Predicatul adun/3
adun(N, M, R) :- sum_lists(N, M, 0, Res), Res = R.

% Funcție auxiliară care adună două liste cifră cu cifră, gestionând și transportul.
sum_lists([], [], 0, []) :- !.
sum_lists([], [], Carry, [Carry]) :- Carry > 0, !.
sum_lists([H|T], [], Carry, [RH|RT]) :- !,
    Sum is H + Carry,
    RH is Sum mod 10,
    NewCarry is Sum div 10,
    sum_lists(T, [], NewCarry, RT).
sum_lists([], [H|T], Carry, [RH|RT]) :- !,
    sum_lists([H|T], [], Carry, [RH|RT]).
sum_lists([H1|T1], [H2|T2], Carry, [RH|RT]) :-
    Sum is H1 + H2 + Carry,
    RH is Sum mod 10,
    NewCarry is Sum div 10,
    sum_lists(T1, T2, NewCarry, RT).


% Predicatul p1/3 care acceptă A, B și C, verifică sau generează B având A și C.
p1(A, B, C) :-
    % Asigurăm că A și C sunt cunoscute (nu sunt variabile neinstantiate)
    nonvar(A), nonvar(C),
    % Generăm B știind că A + B = C
    B is C - A.

% Sau, dacă dorim să verificăm suma:
p1(A, B, C) :-
    % Verificăm dacă suma lui A și B este egală cu C, presupunând că toate sunt cunoscute
    C is A + B.
