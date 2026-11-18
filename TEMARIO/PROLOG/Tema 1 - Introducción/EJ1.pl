qsort([], []).
qsort([P|Xs], Ordenada) :-
particion(P, Xs, Menores, Mayores),
qsort(Menores, OrdMenores),
qsort(Mayores, OrdMayores),
append(OrdMenores, [P|OrdMayores], Ordenada).
particion(_, [], [], []).
particion(P, [X|Xs], [X|Ms], Gs) :- X >= P, !, particion(P, Xs, Ms, Gs).
particion(P, [X|Xs], Ms, [X|Gs]) :- particion(P, Xs, Ms, Gs).

qsort([3, 1, 4, 1, 5, 9, 2, 6], L).