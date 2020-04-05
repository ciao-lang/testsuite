:- module(_,_,[assertions, nativeprops]).

:- texec p(X) : (X=0) + not_fails.

p(_).

% warning not shown
