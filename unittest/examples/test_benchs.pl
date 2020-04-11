:- module(_,_,[assertions, nativeprops]).

% module for testing features not covered by test_examples.pl and
% other modules

%%%%%%%%%%%%%%%%%%
%%%% timeouts %%%%
%%%%%%%%%%%%%%%%%%

% timeout in predicate
:- texec p(X) + timeout(100).
p(X) :- p(X).

% timeout in generation
:- texec q(X) : gen(X) + timeout(100).
q(_).
gen(X) :- gen(X).

% timeout in postcondition
:- test r(X) => post(X) + timeout(100).
r(_).
post(X) :- post(X).

% timeout in predicate + failure/exception properties
:- test p(X) + (timeout(100), not_fails).
:- test p(X) => post(X) + (timeout(100), no_exception).
:- test s(X) + (timeout(100), exception(my_exception)).
s(X) :- s(X), throw(my_exception).
