:- module(_,_,[assertions]).

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(unittest), [run_tests/3]).
:- use_module(library(regrtest/regrtest_aux),[clean_output/3]).
:- use_module(library(stream_utils), [write_string/1]).
:- use_module(library(process),[process_call/3]).

run_tests_in_benchmarks(Opts, Actions) :-
    bundle_path(testsuite, 'unittest/examples', AbsDir),
    Queries = [
        use_module(library(unittest)),
        run_tests(AbsDir, [dir_rec|Opts], Actions)
        ],
    process_call(path(ciaosh),
                 ['-q', '-f'],
                 [stdin(terms(Queries)),
                  stderr(stdout),
                  stdout(string(Out))
                 ]),
    clean_output(OutClean,Out,[]),
    write_string(OutClean).

% TODO: more coverage, both in tests in this module and in benchmarks
% in examples/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Testing unittest interface: all predicates are based on run_tests/3
% (tested below) and are really simple. No tests for now

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Regression on this test can only fail because of changes in the code
% that dumps saved tests results.
:- test run_tests_in_benchmarks(Opts, Actions)
   : (Opts=[], Actions=[saved_vers, show_results])
   # "Test showing saved results".

% Provided previous tests passed (and good coverage from benchmarks),
% regression of this test can only fail because of changes in the code
% that saves tests results, and tests effectively that code if the
% benchmarks provide enough coverage.
:- test run_tests_in_benchmarks(Opts, Actions)
   : (Opts=[], Actions=[check, show_results])
   # "Test showing tests results, stats, and stdout/stderr".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
% Other tests

:- test run_tests_in_benchmarks(Opts, Actions)
   : (Opts=[], Actions=[check, briefcompare])
   # "Test check + briefcompare".

:- test run_tests_in_benchmarks(Opts, Actions)
   : (Opts=[], Actions=[check, compare])
   # "Test check + compare".

:- test run_tests_in_benchmarks(Opts, Actions)
   : (Opts=[rtc_entry], Actions=[check, briefcompare])
   # "Test rtc_entry + check + briefcompare".

:- test run_tests_in_benchmarks(Opts, Actions)
   : (Opts=[rtc_entry], Actions=[check, compare])
   # "Test rtc_entry + check + compare".

% TODO: we could make a safe bootstrap of the test system if we
% separate the dynamic compilation. For example including unittest in
% ciao-boot statically.
