#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-

:- use_module(library(unittest), [run_tests/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

main(_) :-
    bundle_path(testsuite, 'unittest/examples', AbsDir),
    run_tests(AbsDir, [dir_rec], [check, show_results]).
