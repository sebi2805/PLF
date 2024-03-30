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
