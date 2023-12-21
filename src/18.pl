:- use_module(library(csv)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).

direction("R", right).
direction("L", left).
direction("U", up).
direction("D", down).

next_corner(right-Dist, X-Y, X2-Y) :-
    X2 #= X + Dist.
next_corner(up-Dist, X-Y, X-Y2) :-
    Y2 #= Y - Dist.
next_corner(down-Dist, X-Y, X-Y2) :-
    Y2 #= Y + Dist.
next_corner(left-Dist, X-Y, X2-Y) :-
    X2 #= X - Dist.

run(_, [], A, A).
run(X-Y, [Dir-Num|Data], A-Acc, Sol) :-
    next_corner(Dir-Num, X-Y, X1-Y1),
    Acc1 #= Acc + Num,
    run(X1-Y1, Data, [X1-Y1|A]-Acc1, Sol).

picksTheorem(Area, I, B) :-
    Area #= I + (B div 2) - 1.

shoelace_([_], Sol, Sol1) :-
    Sol1 #= abs(Sol) div 2.
shoelace_([X1-Y1, X2-Y2|More], Acc, Sol) :-
    Acc1 #= Acc + (X1*Y2) - (Y1*X2),
    shoelace_([X2-Y2|More], Acc1, Sol).
shoelace([K|Keys], Acc, Sol) :-
    append([K|Keys], [K], Keys1),
    shoelace_(Keys1, Acc, Sol).

part1(File, Sol) :-
    phrase_from_file(parse_csv(frame([], Data), [with_header(false), token_separator(' ')]), File),
    maplist(\ [Dir, Num, _]^(Dir1-Num)^direction(Dir, Dir1), Data, Data1),
    run2(0-0, Data1, []-0, Keys-B),
    shoelace(Keys, 0, Area),
    picksTheorem(Area, I, B),
    Sol #= I + B.

direction_num('0', right).
direction_num('1', down).
direction_num('2', left).
direction_num('3', up).

part2(File, Sol) :-
    phrase_from_file(parse_csv(frame([], Data), [with_header(false), token_separator(' ')]), File),
    maplist(\ [_, _, [_,_,D1,D2,D3,D4,D5,D6,_]]^(Dir1-Num)^(
                direction_num(D6, Dir1),
                % Prolog understands hex out of the box
                number_chars(Num, ['0', 'x', D1,D2,D3,D4,D5])
            )
            , Data
            , Data1),
    run(0-0, Data1, []-0, Keys-B),
    shoelace(Keys, 0, Area),
    picksTheorem(Area, I, B),
    Sol #= I + B.
