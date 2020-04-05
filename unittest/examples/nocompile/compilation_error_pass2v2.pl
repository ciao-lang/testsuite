:- module(_,_,[assertions]).

:- test p(X) : (X=0) => non_defined_prop(X).

p(_).

% This error is only detected in the module wrapper. At that point,
% locators are meaningless since that module is expanded by means of a
% package, and the error can not be interpreted by looking at the
% code. If instead of a non-defined predicate error with a
% distinguishable name like non_defined_prop/1, we get it for example
% for ./2 because we are calling a list, it is very difficult to debug.
