:- bundle(testsuite).
% A testsuite for Ciao
version('1.19.0').
depends([
  ciaodbg
]).
alias_paths([
  testsuite_iso_tests = iso_tests,
  testsuite_ecrc = ecrc,
  testsuite_actmod = actmod
]).
