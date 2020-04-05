:- module(_,_,[assertions]).

:- test p(X) : (X=0) => int(X).

p(X) :- non_defined_pred(X).
