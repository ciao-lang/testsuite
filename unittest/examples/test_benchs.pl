:- module(_,_,[assertions, nativeprops]).

% module for testing features not covered by test_examples.pl and
% other modules

%%%%%%%%%%%%%%%%%%
%%%% timeouts %%%%
%%%%%%%%%%%%%%%%%%

% timeout in predicate
:- texec p(X) + timeout(1000).
p(X) :- p(X).

% timeout in generation
:- texec q(X) : gen(X) + timeout(1000).
q(_).
gen(X) :- gen(X).

% timeout in postcondition
:- test r(X) => post(X) + timeout(1000).
r(_).
post(X) :- post(X).
