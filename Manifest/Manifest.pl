:- bundle(testsuite).
% A testsuite for Ciao
version('1.23.0'). % (same as 'core')
depends([
  ciaodbg_extra
]).
alias_paths([
  testsuite_ecrc = ecrc,
  testsuite_actmod = actmod
]).
