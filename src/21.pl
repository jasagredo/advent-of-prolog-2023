:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(clpz)).
:- use_module(library(assoc)).
:- use_module(library(ordsets)).
:- use_module(library(pio)).

%%% Utils
repeatN(0, _, In, In).
repeatN(N, G_2, In, Out) :-
    N #> 0,
    N1 #= N - 1,
    call(G_2, In,Aux),
    repeatN(N1, G_2, Aux, Out).

eos([], []).

matrix(_-_, A, A, _) --> call(eos).
matrix(_-Y, A0, A1, ZZ) -->
    "\n",
    { Y1 #= Y + 1 },
    matrix(0-Y1, A0, A1, ZZ).
matrix(X-Y, A0, A1, WW) -->
    [Char],
    { dif(Char, '\n'), put_assoc(X-Y, A0, Char, A2), X1 #= X + 1 },
    matrix(X1-Y, A2, A1, ZZ),
    { Char = 'S' -> WW = X-Y ; WW = ZZ }.

%% Movements
right(X-Y, MaxX-_, X1-Y) :-
    X #< MaxX - 1,
    X1 #= X + 1.
left(X-Y, _, X1-Y) :-
    X #> 0,
    X1 #= X - 1.
down(X-Y, _-MaxY, X-Y1) :-
    Y #< MaxY - 1,
    Y1 #= Y + 1.
up(X-Y, _, X-Y1) :-
    Y #> 0,
    Y1 #= Y - 1.

move(MaxX-MaxY, A, X-Y, L) :-
    maplist(\G^LL^(
                call(G, X-Y, MaxX-MaxY, R),
                get_assoc(R, A, '.')
             -> LL = [R]
             ;  LL = []
            ),
            [left, up, right, down],
            LS),
    append(LS, List),
    list_to_ord_set(List, L).

run(MaxX-MaxY, A, s(EvenLocations-EvenStable, []-OddStable),
    s(NewOddLocations-OddStable, []-NewEvenStable)
   ) :-
    maplist(move(MaxX-MaxY, A), EvenLocations, OddLocations),
    ord_union(OddLocations, OddLocations1),
    ord_subtract(OddLocations1, OddStable, NewOddLocations),
    ord_union(EvenLocations, EvenStable, NewEvenStable).

part1(File, Sol) :-
    phrase_from_file(matrix(0-0, t, A, SX-SY), File),
    repeatN(64,
            \W1^W2^(run(131-131, A, W1, W2)),
            s([SX-SY]-[], []-[]),
            s(Sol1-Sol2, _)
           ),
    ord_union(Sol1, Sol2, SolL),
    length(SolL, Sol).

%% Part 2 is still unsolved
