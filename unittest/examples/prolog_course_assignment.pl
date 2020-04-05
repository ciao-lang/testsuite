% :- module(_, _, [assertions, doccomments]).
:- module(_, _, [assertions, doccomments, 'bf/bfall']).
% :- use_package('bf/bfall').

% TODO: translate to english

%! \title  Práctica de programación lógica pura: edificios
%  \author Manuel Hermenegildo
%  \module
%  
%   En esta práctica representaremos edificios de viviendas, con
%   varios niveles, y con varias viviendas en cada nivel. La
%   estructura de datos que utilizaremos será una lista formada por
%   los diferentes niveles, donde cada nivel es a su vez una lista de
%   las viviendas de ese nivel. Los valores almacenados representan el
%   número de personas que viven en cada vivienda, para lo que
%   utilizaremos números naturales, en notación de Peano.
% 
%   Con esta representación, el término:
% 
%                     [ [ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ]
% 
%   representa el edificio:
%   ```
%                                     Vivienda 1  Vivienda 2
%                       -----------------------------------------
%                       Nivel 2        0          s(s(0))
%                       Nivel 1        s(0)       s(s(s(0)))
%   ```
% 
% Los edificios pueden tener un número arbitrario de niveles y de
% viviendas en cada nivel. 

%! basic_building(X): `X` es un edificio, que contiene números
%   naturales. Debe tener al menos un nivel y cada nivel al menos una
%   vivienda. \includedef{basic_building/1}

basic_building([L]) :-
    level_nats(L, _).
basic_building([L|Ls]) :-
    level_nats(L, _),
    basic_building(Ls).

:- test basic_building(X) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]]) + not_fails # "Test 1.1: Un edificio simple.".
:- test basic_building(X) : (X = [[0, 0], [0, 0]]) + not_fails # "Test 1.2: Se admiten pisos vacíos (1).".
:- test basic_building(X) : (X = [[0], [0]]) + not_fails # "Test 1.3: Se admiten pisos vacíos (2).".
:- test basic_building(X) : (X = [[], [0, s(s(0))]]) + fails # "Test 1.4: Al menos una vivienda (1).".
:- test basic_building(X) : (X = [[]]) + fails # "Test 1.5: Al menos una vivienda (2).".
:- test basic_building(X) : (X = []) + fails # "Test 1.6: Al menos un nivel.".
:- test basic_building(X) : (X = [[s(0), a], [0, s(s(0))]]) + fails # "Test 1.7: Contenido no permitido".
:- test basic_building(X) : (X = [[s(0), s(s(s(0)))], [0, s(s(0)), s(s(0)), s(s(s(s(0))))]]) + not_fails # "Test 1.8: Distinto número de pisos.".
:- test basic_building(X) : (X = [[Y|Z]]) => (nat(Y), list(nat, Z)) + (not_fails, try_sols(1)) # "Test 1.9: Lista cerrada (1).".
:- test basic_building(X) : (X = [[s(0), Y]]) => (nat(Y)) + (not_fails, try_sols(1)) # "Test 1.10: Lista cerrada (2)".
:- test basic_building(X) : (X = [[s(0)|Y]]) => (list(nat, Y)) + (not_fails, try_sols(1)) # "Test 1.11: Lista cerrada (3).".

%% -------------------------------------------------------------------
%! level_nats(X,N): `X` es un nivel, con al menos una vivienda, y que 
%   contiene números naturales. \includedef{level_nats/2}

level_nats([N], s(0)) :-
    nat(N).
level_nats([N|Xs], s(V)) :-
    nat(N),
    level_nats(Xs, V).

:- test level_nats(X, Y) : (X = [s(0), s(0), s(0)]) => (Y = s(s(s(0)))) + not_fails.
% :- test level_nats(X, Y) : (X = [   _,    _,    _]) => (Y = s(s(s(0)))) + not_fails.
:- test level_nats(X, Y) : (X = [s(0), s(s(s(0)))]) => (Y = s(s(0))).
:- test level_nats(X, Y) : (X = [s(s(s(0)))], Y = s(s(0))) + fails.
:- test level_nats(X, Y) : (X = [], Y = 0) + fails.
:- test level_nats(X, Y) : (X = []) + fails.
:- test level_nats(X, Y) : (Y = 0) + fails.

%% -------------------------------------------------------------------
%! building(X): `X` es un edificio, que contiene números naturales, y
%   en el que todos los niveles tienen el mismo número de viviendas, y
%   al menos una por por nivel. 
%   \includedef{building/1} 

building([L|Ls]) :-
    level_nats(L, V),
    building_(Ls, V).

%! building_/2: \includedef{building_/2} 

building_([], _).
building_([L|Ls], V) :-
    level_nats(L, V),
    building_(Ls, V).

:- test building(X) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]]) + not_fails # "Test 2.1: Un edificio.".
:- test building(X) : (X = [[0, 0], [0, 0]]) + not_fails # "Test 2.2: Se admiten pisos vacíos (1).".
:- test building(X) : (X = [[0], [0]]) + not_fails # "Test 2.3: Se admiten pisos vacíos (2).".
:- test building(X) : (X = [[], [0, s(s(0))]]) + fails # "Test 2.4: Al menos una vivienda (1).".
:- test building(X) : (X = [[]]) + fails # "Test 2.5: Al menos una vivienda (2).".
:- test building(X) : (X = []) + fails # "Test 2.6: Al menos un nivel.".
:- test building(X) : (X = [[s(0), a], [0, s(s(0))]]) + fails # "Test 2.7: Contenido no permitido".
:- test building(X) : (X = [[s(0)|L], [0]]) => (L = []) + (not_fails, try_sols(1)) # "Test 2.8: Lista cerrada.".
:- test building(X) : (X = [[s(0)|Y]]) => (list(nat, Y)) + (not_fails, try_sols(1)) # "Test 2.9: Lista cerrada.".
:- test building(X) : (X = [[0, _, _], [s(0)|L]]) => (L = [A, B], list(nat, L)) + (not_fails, try_sols(1)) # "Test 2.10: Lista cerrada.".
:- test building(X) : (X = [[0, _, _], [s(0)|L1], [s(0)|L2]]) => (L1 = [_, _], list(nat, L1), L2 = [_, _], list(nat, L2)) + (not_fails, try_sols(1)) # "Test 2.11: Mismo número de viviendas por nivel.".
:- test building(X) : (X = [[Y|Z]]) => (nat(Y), list(nat, Z)) + (not_fails, try_sols(1)) # "Test 2.12: Lista cerrada (1).".
:- test building(X) : (X = [[s(0), Y]]) => ((nat(Y))) + (not_fails, try_sols(1)) # "Test 2.13: Lista cerrada (2).".
:- test building(X) : (var(X)) => (X = [[_|_]|_], list(list(nat), X)) + (not_fails, try_sols(1)) # "Test 2.14: Generar edificio válido.".
:- test building(X) : (var(X)) => (X = [[_|_]|_], list(list(nat), X)) + (not_fails, try_sols(3)) # "Test 2.15: Generar al menos 3 edificios válidos.".

%% -------------------------------------------------------------------
%! nat(N): `N` es un número natural. \includedef{nat/1} 

%  nat(N): `N` is a natural number. \includedef{nat/1} 

nat(0).
nat(s(N)) :-
    nat(N).

%% -------------------------------------------------------------------
%! level(X,N,C): `C` es el nivel `N`-ésimo del edificio `X` (la lista
%   con todas las viviendas de ese nivel). \includedef{level/3}

% level(Building,N,Level) `Level` is the `N`th level of `Building`.

level([X|__], s(0), X).
level([_|Xs], s(N), Level) :-
    level(Xs, N, Level).

%! typed_level/3: Como `level/3`, pero tipado. \includedef{typed_level/3}

typed_level(B, N, C) :-
    building(B),
    nat(N),
    level_nats(C, _),
    level(B, N, C).

:- test level(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=s(0)) => (L = [s(0), s(s(s(0)))]) + not_fails # "Test 3.1: Acceder a un nivel (1).".
:- test level(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], L = [s(0), s(s(s(0)))]) => (N=s(0)) + not_fails # "Test 3.2: Ver en número de un nivel.".
:- test level(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=s(s(0))) => (L = [0, s(s(0))]) + not_fails # "Test 3.3: Acceder a un nivel (2).".
:- test level(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=0) + fails # "Test 3.4: Al menos un nivel.".
:- test level(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=s(s(s(0)))) + fails # "Test 3.5: Acceder a un nivel que no existe (1).".
:- test level(X,N,L) : (X = []) + fails # "Test 3.6: No se admiten edificios sin niveles.".
% :- test level(X,N,L) : (L = []) + fails. %% fallo infinito
:- test level(X,N,L) : (N = 0) + fails # "Test 3.7: Acceder a un nivel que no existe (2).".
:- test level(X,N,L) : (X = [[0], [0]], N=s(s(0))) => (L=[0]) + not_fails # "Test 3.8: Acceder a un nivel (3).".
% :- test level(X,N,L) : (X = [[],[]]) + fails. % Works for typed_


%% -------------------------------------------------------------------
%! column(X,N,C): `C` es la lista formada por las viviendas `N`-ésimas
%   de todos los niveles del edificio `X`. Es decir, si `X` está
%   instanciada al edificio ejemplo de arriba, la consulta
%   `column(X,s(0),C)` respondería `C = [s(0), 0]`.
%   \includedef{column/3}

%% column(Building,N,Col): `Col` is the `N`th column of `Building`.

column([X|Xs], N, Col) :-
    column_([X|Xs], N, Col).

%! column_/3: \includedef{column_/3}

column_([], _, []).
column_([X|Xs], N, [C|Cs]) :-
    element_at(X, N, C),
    column_(Xs, N, Cs).

%! typed_column/3: Como column/3 pero tipado. \includedef{typed_column/3}

typed_column([X|Xs], N, Col) :-
    building([X|Xs]),
    nat(N),
    column_([X|Xs], N, Col).

:- test column(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=s(0)) => (L = [s(0), 0]) + not_fails # "Test 4.1: Seleccionar una columna (1).".
:- test column(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=s(s(0))) => (L = [s(s(s(0))), s(s(0))]) + not_fails # "Test 4.2: Seleccionar una columna (2).".
:- test column(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=0) + fails # "Test 4.3: Se accede a una columna que no existe (1).".
:- test column(X,N,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]], N=s(s(s(0)))) + fails # "Test 4.4: Se accede a una columna que no existe (2).".
:- test column(X,N,L) : (X = []) + fails # "Test 4.5: El edificio no puede estar vacío.".
:- test column(X,N,L) : (X = [[0], [0]], N=0) + fails # "Test 4.6: Se accede a una columna que no existe (3).".
:- test column(X,N,L) : (X = [[0], [0]], N=s(0)) => (L=[0,0]) + not_fails # "Test 4.7: Seleccionar una columna (3).".
:- test column(X,N,L) : (X = [[0], [0]], N=s(s(0))) => (L=[0,0]) + fails # "Test 4.8: Se accede a una columna que no existe (4).".
:- test column(X,N,L) : (X = [[0], [0]], N=s(s(0))) + fails # "Test 4.9: Se accede a una columna que no existe (5).".
% :- test column(X,N,L) : (X = [[],[]]) + fails. % Works for typed_

%% -------------------------------------------------------------------
%! element_at(List,N,Element): `Element` es el elemento `N`-ésimo de
%   la lista `List.` \includedef{element_at/3}

% `Element` is the `N`th element of `List`.

element_at([E|__], s(0), E).
element_at([_|Es], s(N), E) :-
    element_at(Es, N, E).

%% -------------------------------------------------------------------
%! columns(X,C): `C` es la lista de las columnas de viviendas del
%   edificio `X`. \includedef{columns/2}

columns([L|Ls], Cs) :-
    level_nats(L, N),
    columns_([L|Ls], s(0), s(N), Cs).

%! columns_/4: \includedef{columns_/4}

columns_(_B, Max, Max, []).
columns_(B, N, Max, [C|Cs]) :-
    column(B, N, C),
    columns_(B, s(N), Max, Cs).

:- test columns(X,L) : (X = [[s(0), s(s(s(0)))], [0, s(s(0))]]) => (L = [[s(0),0],[s(s(s(0))),s(s(0))]]) + not_fails # "Test 5.1: Obtener todas las columnas (1).".
:- test columns(X,L) : (X = [[s(0)], [0]]) => (L = [[s(0),0]]) + not_fails # "Test 5.2: Obtener todas las columnas (2).".
:- test columns(X,L) : (X = []) + fails # "Test 5.3: El edificio no puede estar vacío.".
:- test columns(X,L) : (X = [[0], [0]]) => (L = [[0,0]]) + not_fails # "Test 5.4: Obtener todas las columnas (3).".
:- test columns(X,L) : (X = [[0, s(0)], [0, s(s(0))]]) => (L = [[0,0], [s(0),s(s(0))]]) + not_fails # "Test 5.5: Obtener todas las columnas (4).".
:- test columns(X,L) : (X = [[0],[s(0)], [0], [s(s(0))]]) => (L = [[0,s(0),0,s(s(0))]]) + not_fails # "Test 5.6: Obtener todas las columnas (5).".
:- test columns(X,L) : (X = [[s(s(0)), s(0), s(s(s(0)))], [0, s(s(0)), s(0)]]) => (L = [[s(s(0)), 0], [s(0),s(s(0))], [s(s(s(0))),s(0)]]) + not_fails # "Test 5.7: Obtener todas las columnas (6).".
:- test columns(X,L) : (X = [[s(s(0)), s(0)], [0, s(s(0))], [s(0), s(s(s(0)))]]) => (L = [[s(s(0)), 0, s(0)], [s(0),s(s(0)), s(s(s(0)))]]) + not_fails # "Test 5.8: Obtener todas las columnas (7).".
:- test columns(X,L) : (X = [[s(s(0)), s(0), s(s(s(0)))], [0, s(s(0)), s(0)], [s(0), s(s(s(0))), s(s(0))]]) => ( L=[[s(s(0)),0,s(0)],[s(0),s(s(0)),s(s(s(0)))],[s(s(s(0))),s(0),s(s(0))]]) + not_fails # "Test 5.9: Obtener todas las columnas (8).".
:- test columns(X,L) : (X = [[s(s(0)), s(0), s(s(s(0)))], [0, s(s(0))]]) + fails # "Test 5.10: Mismo número de viviendas por nivel.".

%% -------------------------------------------------------------------
%! total_people(X,T): `T` es el número total de personas que viven en
%   el edificio `X`. \includedef{total_people/2}

total_people(X, T) :-
    totals(X, T, _).

%! totals/3: \includedef{totals/3}

totals(X, TP, TA) :-
    building(X),
    totals_(X, 0, TP, 0, TA).

%! totals_/5: \includedef{totals_/5}

totals_([], TP, TP, TA, TA).
totals_([L|Ls], P, TP, A, TA) :-
    level_totals(L, 0, LP, 0, LA),
    add(P, LP, NP),
    add(A, LA, NA),
    totals_(Ls, NP, TP, NA, TA).

%! level_totals/5: \includedef{level_totals/5}

level_totals([], TP, TP, TA, TA).
level_totals([N|Vs], P, TP, A, TA) :-
    add(P, N, NP),
    level_totals(Vs, NP, TP, s(A), TA).

%! add/3: \includedef{add/3}

add(0, X, X) :- nat(X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

:- test total_people(X,L)
   : ( X = [[0], [0], [0]], L = s(_)) + fails # "Test 6.1: Número incorrecto de personas.".
:- test total_people(X,L)
   : ( X = [[0], [0], [0]] ) => ( L = 0 ) + not_fails # "Test 6.2: Edificio sin personas.".
:- test total_people(X,L)
   : ( X = [[], [], []] ) + fails # "Test 6.3: Al menos una vivienda.".
:- test total_people(X,L)
   : ( X = [[s(0), s(s(s(0)))], [0, s(s(0))]] ) => ( L = s(s(s(s(s(s(0)))))) ) + not_fails # "Test 6.4: Calcular número de personas (1).".
:- test total_people(X,L)
   : ( X = [[s(0), s(s(s(0)))], [0, s(0), s(s(0))]] ) + fails # "Test 6.5: Mismo número de viviendas por nivel.".
:- test total_people(X,L)
   : ( X = [[s(0), s(s(s(0))), s(0)], [0, s(0), no]] ) + fails # "Test 6.6: Contenido incorrecto en nivel.".
:- test total_people(X,L)
   : ( X = [[s(0), s(s(s(0)))], [0, s(s(0))]], L = s(s(s(s(s(0))))) ) + fails # "Test 6.7: Detectar número incorrecto de personas.".
:- test total_people(X,L)
   : ( X = [[s(0), s(s(s(0)))], [0, s(s(0))], [s(0),0]] ) => ( L = s(s(s(s(s(s(s(0))))))) ) + not_fails # "Test 6.8: Calcular número de personas (2).".

%%% Alt: people only
%% 
%% total_people(X,T) :- 
%%     total_people_(X,0,T).
%% 
%% total_people_([],T,T).
%% total_people_([L|Ls],A,T) :- 
%%     total_level(L,0,TL),
%%     add(A,TL,NA),
%%     total_people_(Ls,NA,T).
%% 
%% total_level([],T,T).
%% total_level([V|Vs],A,T) :-
%%     add(V,A,NA),
%%     total_level(Vs,NA,T).


%% -------------------------------------------------------------------
%! average(X,A): `A` es la media de personas que viven en cada
%   vivienda del edificio, redondeada al número natural más cercano.

average(X, A) :-
    building(X),
    totals(X, TP, TA),
    nat_div(TP, TA, 0, A).

%! nat_div/4: \includedef{nat_div/4}

nat_div(X, Y, T, T) :-
    less(X, Y).
nat_div(X, Y, N, T) :-
    add(X1, Y, X),
    nat_div(X1, Y, s(N), T).

%! less/2: \includedef{less/2}

less(0, s(X)) :- nat(X).
less(s(X), s(Y)) :- less(X, Y).

:- test average(X,A) 
   : ( X = [[0], [0], [0]] ) => ( A = 0 ) + not_fails # "Test 7.1: No hay nadie.".
:- test average(X,A)
   : ( X = [[], [0], [0]] ) + fails # "Test 7.2: Nivel vacío.".
:- test average(X,A)
   : ( X = [[s(0)], [s(0)], [s(0)]] ) => ( A = s(0) ) + not_fails # "Test 7.3: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0)], [s(0)], [s(s(0))]] ) => ( A = s(0) ) + not_fails # "Test 7.4: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0)], [s(0)], [s(s(s(0)))]] ) => ( A = s(0) ; A = s(s(0))) + not_fails # "Test 7.5: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0)], [s(0)], [s(s(s(s(0))))]] ) => ( A = s(s(0)) ) + not_fails # "Test 7.6: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0)], [s(0)], [s(s(s(s(s(0)))))]] ) => ( A = s(s(0)) ) + not_fails # "Test 7.7: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0)], [s(0)], [s(s(s(s(s(s(0))))))]] ) => ( A = s(s(0)) ; A = s(s(s(0))) ) + not_fails # "Test 7.8: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0), s(s(s(0)))], [0, s(s(0))]] ) => ( A = s(0) ) + not_fails # "Test 7.9: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0), s(s(s(0)))], [0, s(s(s(0)))]] ) => ( A = s(0) ; A = s(s(0)) ) + not_fails # "Test 7.10: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0), s(s(s(0)))], [0, s(s(s(s(0))))]] ) => ( A = s(0) ; A = s(s(0)) ) + not_fails # "Test 7.11: Calcular una media.".
:- test average(X,A)
   : ( X = [[s(0), s(s(s(0)))], [0, s(s(s(s(s(0)))))]] ) => ( A = s(s(0)) ) + not_fails # "Test 7.12: Calcular una media.".
:- test average(X,A)
   : ( X = [[B]], A = 0 ) => ( B=0 ) + (not_fails, try_sols(1)) # "Test 7.13: Backwards for det avg.".
:- test average(X,A)
   : ( X = [[B]], A = s(s(0)) ) => ( B=s(s(0)) ) + (not_fails, try_sols(1)) # "Test 7.14: Backwards for det avg.".

%% --------------------------------------------------------
%% first_column(?Building,?Column,?Building0)
%% `Building` and `Building0` differ only in the first column,
%% which is `Column`. If `Building` is a one column building
%% then `Building0` is an empty building, i.e., [].
%% --------------------------------------------------------
%% first_column([[E|Es]|Fs],[E|Col],Rest) :-
%% 	building_rest(Es,M,Rest),
%% 	first_column_(Fs,Col,M).
%% 
%% first_column_([],[],[]).
%% first_column_([[E|Es]|Fs],[E|Col],Rest) :-
%% 	building_rest(Es,M,Rest),
%% 	first_column_(Fs,Col,M).
%% 
%% building_rest([],[],[]).
%% building_rest([X|Xs],M,[[X|Xs]|M]).

% Type checking version:
%
%% first_column([[E|Es]|Fs],[E|Col],Rest) <-
%% 	building_rest(Es,M,Rest,L),
%% 	first_column(Fs,Col,L,M).
%% 
%% first_column([],[],_,[]) <- .
%% first_column([[E|Es]|Fs],[E|Col],L,Rest) <-
%% 	building_rest(Es,M,Rest,L),
%% 	first_column(Fs,Col,L,M).
%% 
%% building_rest([],[],[],0) <- .
%% building_rest([X|Xs],M,[[X|Xs]|M],s(N)) <-
%% 	llength(Xs,N).
