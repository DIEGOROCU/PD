suma(X, Y, Z) :- integer(X), integer(Y), !, Z is X + Y.
suma(X, Y, Z) :- integer(Z), between(0, Z, X), Y is Z - X.

resta(X, Y, Z) :- suma(Y, Z, X).