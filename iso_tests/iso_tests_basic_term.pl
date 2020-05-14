:- module(iso_tests_basic_term, _, [assertions, nativeprops, unittestdecls, iso]).

:- doc(author, "Lorea Galech").
:- doc(author, "R@'{e}my Haemmerl@'{e}").

:- doc(module, "ISO standard tests for Ciao for unifcation, type
testings, term comparison and term creation/decomposition.").

:- reexport(engine(runtime_control)).

:- use_module(testsuite_iso_tests(iso_tests_common), []).
:- reexport(testsuite_iso_tests(iso_tests_common)).

%% 8.2.1.4 These tests are specified in page 65 of the ISO standard. %%%%

%test 1
:- test unify_test1 + not_fails
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard.".

unify_test1:- '='(1, 1).

%test 2
:- test unify_test2(X)
    => (X=1)
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard.".

unify_test2(X) :- '='(X, 1).

%test 3
:- test unify_test3(X, Y)
    => (X=Y)
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard.".

unify_test3(X, Y) :- '='(X, Y).

%test 4
:- test unify_test4 + not_fails
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard.".

unify_test4 :- '='(_, _).

%test 5 
:- test unify_test5(X, Y) => (X='abc', Y='abc')
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard.".

unify_test5(X, Y) :- '='(X, Y), '='(X, abc).

%test 6
:- test unify_test6(X, Y) => (X='def', Y='def')
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard.".

unify_test6(X, Y) :- '='(X, def), '='(def, Y).

%test 7
:- test unify_test7 + fails
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard.".

unify_test7 :- '='(1, 2).

%test 8
:- test unify_test8 + fails
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. ".

unify_test8 :- '='(1, 1.0).

%test 9
:- test unify_test9 + fails
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. ".

unify_test9 :- '='(g(X), f(f(X))).

%test 10
:- test unify_test10 + fails
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. ".

unify_test10 :- '='(f(X, 1), f(a(X))).

%test 11
:- test unify_test11 + fails
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. ".

unify_test11 :- '='(f(X, Y, X), f(a(X), a(Y), Y, 2)).

%test 12 
:- test unify_test12
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. The result expected for this test in
undefined.".

unify_test12 :- '='(X, a(X)).

%test 13 
:- test unify_test13
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. The result expected for this test in
undefined by the standard.".

unify_test13 :- '='(f(X, 1), f(a(X), 2)).

%test 14 
:- test unify_test14
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. The result expected for this test in
undefined by the standard.".

unify_test14 :- '='(f(1, X, 1), f(2, a(X), 2)).

%test 15 
:- test unify_test15
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. The result expected for this test in
undefined by the standard.".

unify_test15 :- '='(f(1, X), f(2, a(X))).

%test 16 
:- test unify_test16
#

"ISO standard test. This test checks that the predicate =/2 behaves
according to the ISO standard. The result expected for this test in
undefined by the standard.".

unify_test16 :- '='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).



%% 8.2.2.4 These tests are specified in page 66 of the ISO standard. %%%%


%test 1
:- test unify_occurs_test1 + not_fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test1 :- unify_with_occurs_check(1, 1).

%test 2
:- test unify_occurs_test2(X) => (X=1)
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test2(X) :- unify_with_occurs_check(X, 1).

%test 3
:- test unify_occurs_test3(X, Y) => (X=Y)
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test3(X, Y) :- unify_with_occurs_check(X, Y).

%test 4
:- test unify_occurs_test4 + not_fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test4 :- unify_with_occurs_check(_, _).


%test 5
:- test unify_occurs_test5(X, Y) => (X=abc, Y=abc)
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test5(X, Y) :-
    unify_with_occurs_check(X, Y),
    unify_with_occurs_check(X, abc).

%test 6
:- test unify_occurs_test6(X, Y) => (X=def, Y=def)
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test6(X, Y) :- unify_with_occurs_check(f(X, def), f(def, Y)).

%test 7
:- test unify_occurs_test7 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test7 :- unify_with_occurs_check(1, 2).

%test 8
:- test unify_occurs_test8 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test8 :- unify_with_occurs_check(1, 1.0).

%test 9
:- test unify_occurs_test9 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test9 :- unify_with_occurs_check(g(X), f(X)).

%test 10
:- test unify_occurs_test10 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test10 :- unify_with_occurs_check(f(X, 1), f(a(X))).

%test 11
:- test unify_occurs_test11 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test11 :-
    unify_with_occurs_check(f(X, Y, X), f(a(X), a(Y), Y, 2)).

%test 12
:- test unify_occurs_test12 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test12 :- unify_with_occurs_check(X, a(X)).

%test 13
:- test unify_occurs_test13 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test13 :-
    unify_with_occurs_check(f(X, 1), f(a(X), 2)).

%test 14
:- test unify_occurs_test14 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test14 :- unify_with_occurs_check(f(1, X, 1), f(2, a(X), 2)).

%test 15
:- test unify_occurs_test15 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test15 :- unify_with_occurs_check(f(1, X), f(2, a(X))).

%test 16
:- test unify_occurs_test16 + fails
#

"ISO standard test. This test checks that the predicate
unify_with_occurs_check/2 behaves according to the ISO standard.".

unify_occurs_test16 :-
    unify_with_occurs_check(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).


%% 8.2.3.4 These tests are specified in page 67 of the ISO standard. %%%%


%test 1
:- test not_uni_test1 + fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. ".

not_uni_test1 :- '\\='(1, 1).

%test 2
:- test not_uni_test2(X) + fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. ".

not_uni_test2(X) :- \=(X, 1).

%test 3
:- test not_uni_test3(X, Y) + fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. ".

not_uni_test3(X, Y) :- '\\='(X, Y).

%test 4
:- test not_uni_test4 + fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. ".

not_uni_test4 :- \=(_, _).

%test 5
:- test not_uni_test5(X, Y) + fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. ".

not_uni_test5(X, Y) :- \=(f(X, def), f(def, Y)).

%test 6
:- test not_uni_test6 + not_fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard.".

not_uni_test6 :- '\\='(1, 2).

%test 7
:- test not_uni_test7 + not_fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard.".

not_uni_test7:- \=(1, 1.0).

%test 8
:- test not_uni_test8(X) + not_fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard.".

not_uni_test8(X) :- '\\='(g(X), f(f(X))).

%test 9
:- test not_uni_test9(X) + not_fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard.".

not_uni_test9(X) :- \=(f(X, 1), f(a(X))).

%test 10
:- test not_uni_test10(X, Y) + not_fails
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard.".

not_uni_test10(X, Y) :- '\\='(f(X, Y, X), f(a(X), a(Y), Y, 2)).

%test 11 
:- test not_uni_test11(X) 
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. The result expected for this test in
undefined.".

not_uni_test11(X) :- \=(X, a(X)).

%test 12 
:- test not_uni_test12(X) 
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. The result expected for this test in
undefined.".

not_uni_test12(X) :- '\\='(f(X, 1), f(a(X), 2)).

%test 13 
:- test not_uni_test13(X)
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. The result expected for this test in
undefined.".

not_uni_test13(X) :- '\\='(f(1, X, 1), f(2, a(X), 2)).

%test 14 
:- test not_uni_test14(X) 
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. The result expected for this test in
undefined.".

not_uni_test14(X) :- \=(f(2, X), f(2, a(X))).

%test 15 
:- test not_uni_test15(X, Y) 
#

"ISO standard test. This test checks that the predicate '\='/2 behaves
according to the ISO standard. The result expected for this test in
undefined.".

not_uni_test15(X, Y) :- '\\='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).



%% 8.3.1.4 These tests are specified in page 67 of the ISO standard. %%%%

%test 1
:- test var_test1 + fails
#

"ISO standard test. This test checks that the predicate var/1 behaves
according to the ISO standard.".

var_test1 :- var(foo).

%test 2
:- test var_test2(Foo) + not_fails
#

"ISO standard test. This test checks that the predicate var/1 behaves
according to the ISO standard.".

var_test2(Foo) :- var(Foo).

%test 3
:- test var_test3 + fails
#

"ISO standard test. This test checks that the predicate var/1 behaves
according to the ISO standard.".

var_test3 :- foo=Foo, var(Foo).

% test 4
:- test var_test4 + not_fails
#

"ISO standard test. This test checks that the predicate var/1 behaves
according to the ISO standard.".

var_test4 :- var(_).



%% 8.3.2.4 These tests are specified in page 68 of the ISO standard. %%%%

%test 1
:- test atom_test1 + not_fails
#

"ISO standard test. This test checks that the predicate atom/1 behaves
according to the ISO standard.".

atom_test1 :- atom(atom).

%test 2
:- test atom_test2 + not_fails
#

"ISO standard test. This test checks that the predicate atom/1 behaves
according to the ISO standard.".

atom_test2 :- atom('string').

%test 3
:- test atom_test3 + fails
#

"ISO standard test. This test checks that the predicate atom/1 behaves
according to the ISO standard.".

atom_test3 :- atom(a(b)).

%test 4
:- test atom_test4(Var) + fails
#

"ISO standard test. This test checks that the predicate atom/1 behaves
according to the ISO standard.".

atom_test4(Var) :- atom(Var).

%test 5
:- test atom_test5 + not_fails
#

"ISO standard test. This test checks that the predicate atom/1 behaves
according to the ISO standard.".

atom_test5:- atom([]).

%test 6
:- test atom_test6 + fails
#

"ISO standard test. This test checks that the predicate atom/1 behaves
according to the ISO standard.".

atom_test6 :- atom(6).

%test 7
:- test atom_test7 + fails
#

"ISO standard test. This test checks that the predicate atom/1 behaves
according to the ISO standard.".

atom_test7 :- atom(3.3).


%% 8.3.3.4 These tests are specified in page 68 of the ISO standard. %%%%

%test 1
:- test integer_test1 + not_fails
#

"ISO standard test. This test checks that the predicate integer/1
behaves according to the ISO standard.".

integer_test1 :- integer(3).

%test 2
:- test integer_test2 + not_fails
#

"ISO standard test. This test checks that the predicate integer/1
behaves according to the ISO standard.".

integer_test2 :- integer(-3).

%test 3
:- test integer_test3 + fails
#

"ISO standard test. This test checks that the predicate integer/1
behaves according to the ISO standard.".

integer_test3 :- integer(3.3).

%test 4
:- test integer_test4(X) + fails
#

"ISO standard test. This test checks that the predicate integer/1
behaves according to the ISO standard.".

integer_test4(X) :- integer(X).

%test 5
:- test integer_test5 + fails
#

"ISO standard test. This test checks that the predicate integer/1
behaves according to the ISO standard.".

integer_test5 :- integer(atom).


%% 8.3.4.4 These tests are specified in page 68 of the ISO standard. %%%%

%test 1
:- test float_test1 + not_fails
#

"ISO standard test. This test checks that the predicate float/1
behaves according to the ISO standard.".

float_test1 :- float(3.3).

%test 2
:- test float_test2 + not_fails
#

"ISO standard test. This test checks that the predicate float/1
behaves according to the ISO standard.".

float_test2 :- float(-3.3).

%test 3
:- test float_test3 + fails
#

"ISO standard test. This test checks that the predicate float/1
behaves according to the ISO standard.".

float_test3 :- float(3).

%test 4
:- test float_test4 + fails
#

"ISO standard test. This test checks that the predicate float/1
behaves according to the ISO standard.".

float_test4 :- float(atom).

%test 5
:- test float_test5(X) + fails
#

"ISO standard test. This test checks that the predicate float/1
behaves according to the ISO standard.".

float_test5(X) :- float(X).


%% 8.3.5.4 These tests are specified in page 69 of the ISO standard. %%%%

%test 1
:- test atomic_test1 + not_fails
#

"ISO standard test. This test checks that the predicate atomic/1
behaves according to the ISO standard.".

atomic_test1 :- atomic(atom).

%test 2
:- test atomic_test2 + fails
#

"ISO standard test. This test checks that the predicate atomic/1
behaves according to the ISO standard.".

atomic_test2 :- atomic(a(b)).

%test 3
:- test atomic_test3(Var) + fails
#

"ISO standard test. This test checks that the predicate atomic/1
behaves according to the ISO standard.".

atomic_test3(Var) :- atomic(Var).

%test 4
:- test atomic_test4 + not_fails
#

"ISO standard test. This test checks that the predicate atomic/1
behaves according to the ISO standard.".

atomic_test4 :- atomic(6).

%test 5
:- test atomic_test5 + not_fails
#

"ISO standard test. This test checks that the predicate atomic/1
behaves according to the ISO standard.".

atomic_test5 :- atomic(3.3).



%% 8.3.6.4 These tests are specified in page 69 of the ISO standard. %%%%

%test 1
:- test compound_test1 + fails
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test1 :- compound(33.3).

%test 2
:- test compound_test2 + fails
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test2 :- compound(-33.3).

%test 3
:- test compound_test3 + not_fails
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test3 :- compound(-a).

%test 4
:- test compound_test4 + fails
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test4 :- compound(_).

%test 5
:- test compound_test5 + fails
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test5 :- compound(a).

%test 6
:- test compound_test6
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test6 :- compound(a(b)).

%test 7
:- test compound_test7 + fails
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test7 :- compound([]).

%test 8
:- test compound_test8 + not_fails
#

"ISO standard test. This test checks that the predicate compound/1
behaves according to the ISO standard.".

compound_test8 :- compound([a]).


%% 8.3.7.4 These tests are specified in page 69 of the ISO standard. %%%%

%test 1
:- test nonvar_test1 + not_fails
#

"ISO standard test. This test checks that the predicate nonvar/1
behaves according to the ISO standard.".

nonvar_test1 :- nonvar(33.3).

%test 2
:- test nonvar_test2 + not_fails
#

"ISO standard test. This test checks that the predicate nonvar/1
behaves according to the ISO standard.".

nonvar_test2 :- nonvar(foo).

%test 3
:- test nonvar_test3(Foo) + fails
#

"ISO standard test. This test checks that the predicate nonvar/1
behaves according to the ISO standard.".

nonvar_test3(Foo) :- nonvar(Foo).

%test 4
:- test nonvar_test4(Foo) + not_fails
#

"ISO standard test. This test checks that the predicate nonvar/1
behaves according to the ISO standard.".

nonvar_test4(Foo) :- foo=Foo, nonvar(Foo).

%test 5
:- test nonvar_test5 + fails
#

"ISO standard test. This test checks that the predicate nonvar/1
behaves according to the ISO standard.".

nonvar_test5 :- nonvar(_).

%test 6
:- test nonvar_test6 + not_fails
#

"ISO standard test. This test checks that the predicate nonvar/1
behaves according to the ISO standard.".

nonvar_test6 :- nonvar(a(b)).


%% 8.3.8.4 These tests are specified in page 70 of the ISO standard. %%%%

%test 1
:- test number_test1 + not_fails
#

"ISO standard test. This test checks that the predicate number/1
behaves according to the ISO standard.".

number_test1 :- number(3).

%test 2
:- test number_test2 + not_fails
#

"ISO standard test. This test checks that the predicate number/1
behaves according to the ISO standard.".

number_test2 :- number(3.3).

%test 3
:- test number_test3 + not_fails
#

"ISO standard test. This test checks that the predicate number/1
behaves according to the ISO standard.".

number_test3 :- number(-3).

%test 4
:- test number_test4 + fails
#

"ISO standard test. This test checks that the predicate number/1
behaves according to the ISO standard.".

number_test4 :- number(a).

%test 5
:- test number_test5(X) + fails
#

"ISO standard test. This test checks that the predicate number/1
behaves according to the ISO standard.".

number_test5(X) :- number(X).

%% 8.4.1.4 These tests are specified in page 70 of the ISO standard. %%%%


%test 1
:- test termcomparision_test1 + not_fails
#

"ISO standard test. This test checks that the predicate '@=<'/2
behaves according to the ISO standard.".

termcomparision_test1:- '@=<'(1.0, 1).

%test 2
:- test termcomparision_test2 + not_fails
#

"ISO standard test. This test checks that the predicate '@<'/2 behaves
according to the ISO standard.".

termcomparision_test2 :- '@<'(1.0, 1).

%test 3
:- test termcomparision_test3 + fails
#

"ISO standard test. This test checks that the predicate '\\=='/2
behaves according to the ISO standard.".

termcomparision_test3 :- '\\=='(1, 1).

%test 4
:- test termcomparision_test4 + not_fails
#

"ISO standard test. This test checks that the predicate '@=<'/2
behaves according to the ISO standard.".

termcomparision_test4 :- '@=<'(aardvark, zebra).


%test 5
:- test termcomparision_test5 + not_fails
#

"ISO standard test. This test checks that the predicate '@=<'/2
behaves according to the ISO standard.".

termcomparision_test5 :- '@=<'(short, short).

%test 6
:- test termcomparision_test6 + not_fails
#

"ISO standard test. This test checks that the predicate '@=<'/2
behaves according to the ISO standard.".

termcomparision_test6 :- '@=<'(short, shorter).

%test 7
:- test termcomparision_test7 + fails
#

"ISO standard test. This test checks that the predicate '@>='/2
behaves according to the ISO standard.".

termcomparision_test7 :- '@>='(short, shorter).

%test 8
:- test termcomparision_test8 + fails
#

"ISO standard test. This test checks that the predicate '@>'/2 behaves
according to the ISO standard.".

termcomparision_test8 :- '@<'(foo(a, b), north(a)).

%test 9
:- test termcomparision_test9 + not_fails
#

"ISO standard test. This test checks that the predicate '@>'/2 behaves
according to the ISO standard.".

termcomparision_test9 :- '@>'(foo(b), foo(a)).

%test 10 
:- test termcomparision_test10(X, Y) + not_fails
#

"ISO standard test. This test checks that the predicate '@<'/2 behaves
according to the ISO standard.".

termcomparision_test10(X, Y) :- '@<'(foo(a, X), foo(b, Y)).

%test 11 
:- test termcomparision_test11(X, Y)
#

"ISO standard test. This test checks that the predicate '@<'/2 behaves
according to the ISO standard. The result expected is implementation
dependent.".

termcomparision_test11(X, Y) :- '@<'(foo(X, a), foo(Y, b)).

%tets 12
:- test termcomparision_test12(X, X) + not_fails
#

"ISO standard test. This test checks that the predicate '@=<'/2
behaves according to the ISO standard.".

termcomparision_test12(X, X) :- '@=<'(X, X).

%test 13
:- test termcomparision_test13(X, X) + not_fails
#

"ISO standard test. This test checks that the predicate '=='/2 behaves
according to the ISO standard.".

termcomparision_test13(X, X) :- '=='(X, X).

%test 14 
:- test termcomparision_test14(X, Y)
#

"ISO standard test. This test checks that the predicate '@=<'/2
behaves according to the ISO standard. The result expected is
implementation dependent.".

termcomparision_test14(X, Y) :- '@=<'(X, Y).

%test 15
:- test termcomparision_test15(X, Y) + fails
#

"ISO standard test. This test checks that the predicate '=='/2 behaves
according to the ISO standard.".

termcomparision_test15(X, Y) :- '=='(X, Y).

%test 16
:- test termcomparision_test16 + not_fails
#

"ISO standard test. This test checks that the predicate '\=='/2
behaves according to the ISO standard.".

termcomparision_test16 :- \==(_, _).

%test 17
:- test termcomparision_test17 + fails
#

"ISO standard test. This test checks that the predicate '=='/2 behaves
according to the ISO standard.".

termcomparision_test17 :- '=='(_, _).

%test 18 
:- test termcomparision_test18
#

"ISO standard test. This test checks that the predicate '@=<'/2
behaves according to the ISO standard. The result expected is
implementation dependent.".

termcomparision_test18 :- '@=<'(_, _).

% test 19 
:- test termcomparision_test19(X, Y) 
#

"ISO standard test.This test checks that the predicate '@=<'/2 behaves
according to the ISO standard. The result expected is implementation
dependent.".

termcomparision_test19(X, Y) :- '@=<'(foo(X, a), foo(Y, b)).

%% 8.5.1.4 These tests are specified in the page 71 from the ISO standard %%%%

%test 1
:- test functor_test1 + not_fails
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test1 :- functor(foo(a, b, c), foo, 3).


%test 2
:- test functor_test2(X, Y) => (X=foo, Y=3)
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test2(X, Y) :- functor(foo(a, b, c), X, Y).

%test 3
:- test functor_test3(X) => (X=foo(_, _, _))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test3(X) :- functor(X, foo, 3).

%test 4
:- test functor_test4(X) => (X=foo)
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test4(X) :- functor(X, foo, 0).

%test 5
:- test functor_test5(A, B) => (A=mats, B=2)
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test5(A, B) :- functor(mats(A, B), A, B).

%test 6
:- test functor_test6 + fails
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test6 :- functor(foo(a), foo, 2).

%test 7
:- test functor_test7 + fails
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test7 :- functor(foo(a), fo, 1).

%test 8
:- test functor_test8(X, Y) => (X=1, Y=0)
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test8(X, Y) :- functor(1, X, Y).

%test 9
:- test functor_test9(X) => (X=1.1)
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test9(X) :- functor(X, 1.1, 0).

%test 10
:- test functor_test10 + not_fails
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test10 :- functor([_|_], '.', 2).

%test 11
:- test functor_test11 + not_fails
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test11 :- functor([], [], 0).

%test 12 
:- test functor_test12(X, Y)
    + exception(error(instantiation_error, Imp_dep))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test12(X, Y) :- functor(X, Y, 3).

%test 13 
:- test functor_test13(X, N)
    + exception(error(instantiation_error, Imp_dep))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test13(X, N) :- functor(X, foo, N).

%test 14
:- test functor_test14(X)
    + exception(error(type_error(integer, a), Imp_dep))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test14(X) :- functor(X, foo, a).

%test 15 
:- test functor_test15(X)
    + exception(error(type_error(atom, 1.5), Imp_dep))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test15(X) :- functor(X, 1.5, 1).

%test 16 
:- test functor_test16(X)
    + exception(error(type_error(atomic, foo(a)), Imp_dep))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test16(X) :- functor(X, foo(a), 1).

%test 17 
:- test functor_test17(T, X) :
    (current_prolog_flag(max_arity, A), X is A +1)
    + exception(error(representation_error(max_arity), Imp_dep))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test17(T, X) :- functor(T, foo, X).

%test 18 
:- test functor_test18(F, Minus_1) :
    (Minus_1 is (0 -1))
    + exception(error(domain_error(not_less_than_zero, -1), Imp_dep))
#

"ISO standard test. This test checks that the predicate functor/3
behaves according to the ISO standard.".

functor_test18(F, Minus_1) :- functor(F, foo, Minus_1).


%% 8.5.2.4 These tests are specified in the page 72 from the ISO standard %%%%

%test 1
:- test argument_test1 + not_fails
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.".

argument_test1 :- arg(1, foo(a, b), a).

%test 2
:- test argument_test2(X) => (X=a)
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.".

argument_test2(X) :- arg(1, foo(a, b), X).

%test 3
:- test argument_test3(X) => (X=a)
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.".

argument_test3(X) :- arg(1, foo(X, b), a).

%test 4
:- test argument_test4(X, Y) : (X=Y)
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.".

argument_test4(X, Y) :- arg(1, foo(X, b), Y).

%test 5
:- test argument_test5 + fails
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.".

argument_test5 :- arg(1, foo(a, b), b).

%test 6
:- test argument_test6 + fails
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.".

argument_test6 :- arg(0, foo(a, b), foo).

%test 7
:- test argument_test7(N) + fails
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.".

argument_test7(N) :- arg(3, foo(3, 4), N).

%test 8 
:- test argument_test8(X) + exception(error(instantiation_error, Imp_dep))
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.This test should raise an error but just
fails.".

argument_test8(X) :- arg(X, foo(a, b), a).

%test 9  
:- test argument_test9(X) + exception(error(instantiation_error, Imp_dep))
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.This test should raise an error but just
fails.".

argument_test9(X) :- arg(1, X, a).

%test 10  
:- test argument_test10(A)
    + exception(error(type_error(compound, atom), Imp_dep))
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.This test should raise an error but just
fails.".

argument_test10(A) :- arg(0, atom, A).

%test 11 
:- test argument_test11(A) + exception(error(type_error(compound, 3), Imp_dep))
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard.This test should raise an error but just
fails.".

argument_test11(A) :- arg(0, 3, A).

%test 12 
:- test argument_test12(X)
#

"ISO standard test. This test checks that the predicate arg/3 behaves
according to the ISO standard. The result expected for this test in
undefined.".

argument_test12(X) :- arg(1, foo(X), u(X)).


%%%%%%%%%%%%%%%%%%%%%%%% TEST FROM SICTUS AND EDDBALI %%%%%%%%%%%%%%%%%%%%%%%%

%test 13
:- test argument_test13
    + exception(error(domain_error(not_less_than_zero, -3), Imp_dep))
#

"Non ISO standard test. This test checks that the predicate arg/3
behaves according to the ISO standard.".

argument_test13 :- arg(-3, foo(a, b), _).

% test 14 
:- test argument_test14(X)
    + exception(error(type_error(integer, a), Imp_dep))
#

"Non ISO standard test. This test checks that the predicate arg/3
behaves according to the ISO standard.".

argument_test14(X) :- arg(a, foo(a, b), X).

%test 15 
:- test argument_test15(X, Y) => (X=a, Y=b)
#

"Non ISO standard test. This test checks that the predicate arg/3
behaves according to the ISO standard.".

argument_test15(X, Y) :- arg(2, foo(a, f(X, b), c), f(a, Y)).

%tets 16 
:- test argument_test16 + exception(error(type_error(compound, 3), Imp_dep))
#

"Non ISO standard test. This test checks that the predicate arg/3
behaves according to the ISO standard.".

argument_test16 :- arg(1, 3, _).




%% 8.5.3.4 These tests are specified in the page 73 from the ISO standard %%%%

%test 1
:- test univ_test1 + not_fails
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test1 :- '=..'(foo(a, b), [foo, a, b]).

%test 2
:- test univ_test2(X) => (X=foo(a, b))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test2(X) :- '=..'(X, [foo, a, b]).

%test 3
:- test univ_test3(L) => (L=[foo, a, b])
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test3(L) :- '=..'(foo(a, b), L).

%test 4
:- test univ_test4(X, Y) => (X=a, Y=b)
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test4(X, Y) :- '=..'(foo(X, b), [foo, a, Y]).

%test 5
:- test univ_test5 + not_fails
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test5 :- '=..'(1, [1]).

%test 6
:- test univ_test6 + fails
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test6 :- '=..'(foo(a, b), [foo, b, a]).


%test 7 
:- test univ_test7(X, Y) + exception(error(instantiation_error, Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test7(X, Y) :- '=..'(X, Y).

%test 8 
:- test univ_test8(X, Y) + exception(error(instantiation_error, Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test8(X, Y) :- '=..'(X, [foo, a|Y]).


%test 9 
:- test univ_test9(X) + exception(error(type_error(list, [foo|bar]), Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test9(X) :- '=..'(X, [foo|bar]).


%test 10 
:- test univ_test10(X, Foo) + exception(error(instantiation_error, Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test10(X, Foo) :- '=..'(X, [Foo, bar]).

%test 11 
:- test univ_test11(X) + exception(error(type_error(atom, 3), Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test11(X) :- '=..'(X, [3, 1]).

%test 12 
:- test univ_test12(X) + exception(error(type_error(atom, 1.1), Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test12(X) :- '=..'(X, [1.1, foo]).

%test 13
:- test univ_test13(X) + exception(error(type_error(atom, a(b)), Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test13(X) :- '=..'(X, [a(b), 1]).

%test 14 
:- test univ_test14(X) + exception(error(type_error(list, 4), Imp_dep))
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test14(X) :- '=..'(X, 4).

%test 15
:- test univ_test15(X)
#

"ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard. The result expected for this
test in undefined.".

univ_test15(X) :- '=..'(f(X), [f, u(X)]).


%%%%%%%%%%%%%%%%%%%%%%%% TEST FROM SICTUS AND EDDBALI %%%%%%%%%%%%%%%%%%%%%%%%

%test 16 
:- test univ_test16(X) + exception(error(type_error(atomic, f(a)), Imp_dep))
#

"Non ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test16(X) :- '=..'(X, [f(a)]).

%test 17
:- test univ_test17(X)
    + exception(error(domain_error(non_empty_list, []), Imp_dep))
#

"Non ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test17(X) :- '=..'(X, []).

%test 18 
:- test univ_test18(X, L) :
    ( current_prolog_flag(max_arity, MAX),
        N is (MAX+1),
        my_list_of(N, 1, L)
    ) + exception(error(representation_error(max_arity), Imp_dep))
#

"Non ISO standard test. This test checks that the predicate '=..'/2
behaves according to the ISO standard.".

univ_test18(X, L) :- '=..'(X, [f|L]).



%% 8.5.4.4 These tests are specified in the page 74 from the ISO standard %%%%

%test 1
:- test copyterm_test1(X, Y) + not_fails
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test1(X, Y) :- copy_term(X, Y).

%test 2
:- test copyterm_test2(X) + not_fails
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test2(X) :- copy_term(X, 3).

%test 3
:- test copyterm_test3 + not_fails
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test3 :- copy_term(_, a).

%test 4
:- test copyterm_test4(X) => (X=a)
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test4(X) :- copy_term(a+X, X+b).

%test 5
:- test copyterm_test5 + not_fails
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test5 :- copy_term(_, _).

%test 6
:- test copyterm_test6(X, Y, A, B) => (A=B)
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test6(X, Y, A, B) :- copy_term(X+X+Y, A+B+B).

%test 7
:- test copyterm_test7 + fails
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test7 :- copy_term(a, b).

%test 8
:- test copyterm_test8(X) + fails
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard.".

copyterm_test8(X) :- copy_term(a+X, X+b), copy_term(a+X, X+b).

%test 9 
:- test copyterm_test9(X, Y)
#

"ISO standard test. This test checks that the predicate copy_term/2
behaves according to the ISO standard. The result expected for this
test in undefined.".

copyterm_test9(X, Y) :- copy_term(demoen(X, X), demoen(Y, f(Y))).


