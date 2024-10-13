:- bundle(testsuite).
% A testsuite for Ciao
version('1.24.0'). % (same as 'core')
depends([
  core
]).
alias_paths([
  testsuite_ecrc = ecrc,
  testsuite_actmod = actmod
]).
