%employee_with_good_salary([(peter, 1000), (oliver, 1200), (sam, 700), (ben, 15000), (sandra, 4500)], 1200, Res).
% Res = [oliver, ben, sandra]

employee_with_good_salary([], _, []).
employee_with_good_salary([(Name, Salary) |T], Min, [Name|R]):-
          Salary >= Min, 
          employee_with_good_salary(T, Min, R),
          !.
employee_with_good_salary([_|T], Min, R):-
          employee_with_good_salary(T, Min, R).


employee_sum(_, [],[], 0, 0).
employee_sum(Name, [(Name, Salary)| T], T1, Sum ,Number) :-
          employee_sum(Name, T, T1, SumNew, NumberNew),
          Sum is SumNew + Salary, 
          Number is NumberNew + 1,
          !.
employee_sum(Name, [H|T], [H|T1], Sum, Number):-
          employee_sum(Name, T, T1, Sum, Number).


employee_average_salary([], []).
employee_average_salary([(Name, Salary)| T], [(Name, Average)|R]) :-
          employee_sum(Name, [(Name, Salary)| T], NewEmployees, Sum, Number),
          employee_average_salary(NewEmployees, R),
          Average is Sum/Number.



compare_scores(Order, (_, Score1), (_, Score2)) :-
    compare(Order, Score2, Score1).

sort_by_score(UnsortedList, SortedList) :-
    predsort(compare_scores, UnsortedList, SortedList).

sort_employee_average_salary(L, R):-
          employee_average_salary(L, R1), 
          sort_by_score(R1, R).

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

queen(beatrix).
queen(juliana).
queen(wilhelmina).
queen(emma).
king(willem).

parent(X, Y):-
          mother(X, Y);
          father(X, Y).
ruler(X):-
          queen(X);
          king(X).