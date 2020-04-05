#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-

:- use_module(library(unittest), [run_tests/3]).

main(_) :- % run from this module's directory
    run_tests(unittest_tests, [], [check, briefcompare(Result)]),
    (Result=0 -> true ;
        run_tests(unittest_tests, [], [compare]),
        halt(1)
    ).
