:- bundle(testsuite).
% A testsuite for Ciao
version('1.16.0-alpha.3').
depends([
  ciaodbg
]).
alias_paths([
  testsuite_iso_tests = iso_tests,
  testsuite_ecrc = ecrc
]).
