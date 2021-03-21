:- bundle(testsuite).
% A testsuite for Ciao
version('1.20.0').
depends([
  ciaodbg
]).
alias_paths([
  testsuite_ecrc = ecrc,
  testsuite_actmod = actmod
]).
