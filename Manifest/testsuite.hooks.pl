:- module(_, [], [ciaobld(bundlehooks)]).

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
    % Run ISO-prolog tests
    % TODO: acceptance tests?
    runtests_dir(testsuite, 'iso_tests').

:- use_module(ciaobld(ciaoc_aux), [
    % TODO: use bundle defs instead
    invoke_ciaosh_batch/1]).

% Run benchmarks
'$builder_hook'(bench) :- !,
    invoke_ciaosh_batch([
      use_module(testsuite_ecrc(ecrc), [main/1]),
      ecrc:main([])
    ]).

% TODO: also include (or merge) these:
%   core/examples/misc/ (see Makefile) -- too small, need at least scaling
