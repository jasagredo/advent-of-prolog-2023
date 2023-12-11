:- use_module(library(clpz)).
:- use_module(library(between)).

% call as `scryer-prolog src/all -g "run(4),halt."`

run(D) :-
    format("Solving day ~d~n", [D]),
    number_chars(D, Ds),
    (D #< 10, Pad = "0"; Pad = ""),
    append(["src/", Pad, Ds], SrcC),
    atom_chars(Src, SrcC),
    consult(Src),
    format("Part 1: ~n", []),
    append(["inputs/", Ds, ".txt"], DataC),
    atom_chars(Data, DataC),
    time(part1(Data, Sol1)),
    sol1(X),
    format("   ", []), isok(Sol1, X), !,
    format("Part 2: ~n", []),
    time(part2(Data, Sol2)),
    sol2(Y),
    format("   ", []), isok(Sol2, Y).

runall :-
    numlist(1, 11, L),
    maplist(run, L).
