:- bundle(testsuite).
% A testsuite for Ciao
version('1.22.0'). % (same as 'core')
depends([
  ciaodbg
]).
alias_paths([
  testsuite_ecrc = ecrc,
  testsuite_actmod = actmod
]).
