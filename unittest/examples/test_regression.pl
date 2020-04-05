:- module(_,_,[assertions, nativeprops]).

% This tests produce different results when run with rtc_entry
% option. They are used to test regression.


:- use_module(library(streams)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Output regression %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% not a real property, but fits out purposes
:- prop print_stdout(X).
print_stdout(X) :-
    display(X), nl.

:- pred p(X) : print_stdout(calling(X)) => print_stdout(succeeded(X)).
:- test p(X) : (X=f(_)) + not_fails.
p(X) :- display(running(X)), X=f(a), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Error regression %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

% not a real property, but fits out purposes
:- prop print_stderr(X).
print_stderr(X) :-
    display(user_error, X), nl(user_error).

:- pred q(X) : print_stderr(calling(X)) => print_stderr(succeeded(X)).
:- test q(X) : (X=f(_)) + not_fails.
q(X) :- display(user_error,running(X)), X=f(a), nl(user_error).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Results regression %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% from true to failed
:- success r(X) => int(X).
:- test r(X) : (X=a) + not_fails.
r(_).


%%%% from failed to failed but with another rtcheck
:- calls s(X) : int(X). % this one is detected earlier
:- test s(X) : (X=a) => (X=b).
s(_).
s(_).


%%%% from warning to rtcheck
:- comp t(X,Y) + not_fails. % this one is detected earlier
:- test t(X,Y) : (X=a) => (Y=b).
t(_,_) :- fail.

% TODO: more tests
