:- module(_, [], [ciaobld(bundlehooks)]).

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
	% Run ISO-prolog tests
	% TODO: acceptance tests?
	runtests_dir(testsuite, 'iso_tests').
