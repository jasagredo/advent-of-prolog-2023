:- use_module(dcgs/utils).
:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(between)).
:- use_module(library(debug)).

matrix_nth0(X-Y, Mat, E) :-
    nth0(Y, Mat, L),
    nth0(X, L, E).

% reflect(IncomingDirection, Point, OutgoingDirection)
reflect(X, '.', [X]).
reflect(right, '/', [up]).
reflect(up,    '/', [right]).
reflect(left,  '/', [down]).
reflect(down,  '/', [left]).
reflect(right, '\\', [down]).
reflect(up,    '\\', [left]).
reflect(left,  '\\', [up]).
reflect(down,  '\\', [right]).
reflect(left, '-', [left]).
reflect(right, '-', [right]).
reflect(X, '-', [left, right]) :- X = up; X = down.
reflect(up, '|', [up]).
reflect(down, '|', [down]).
reflect(X, '|', [up, down]) :- X = left; X = right.

next_position(right, X-Y, X1-Y) :- X1 #= X + 1.
next_position(left, X-Y, X1-Y) :- X1 #= X - 1.
next_position(up, X-Y, X-Y1) :- Y1 #= Y - 1.
next_position(down, X-Y, X-Y1) :- Y1 #= Y + 1.

run(Dir-X-Y, MaxX-MaxY, Lines, MatSol, Seen, Seen1) :-
    next_position(Dir, X-Y, X1-Y1),
    Xz #= MaxX - 1,
    Yz #= MaxY - 1,
    (
        % when inside the matrix
        X1 in 0..Xz,
        Y1 in 0..Yz,
        % unify with 1
        matrix_nth0(X1-Y1, MatSol, 1),

        % get next step
        matrix_nth0(X1-Y1, Lines, E),
        reflect(Dir, E, Next),

        (
            % only one next direction
            Next = [Dir1],
            (
                % if we have not seen it yet
                maplist(dif(Dir1-X1-Y1), Seen),
                run(Dir1-X1-Y1, MaxX-MaxY, Lines, MatSol, [Dir1-X1-Y1|Seen], Seen1)
            ;
                % if we have seen it, do nothing
                Seen1 = Seen
            )
        ;
          % two directions (a splitter)
          Next = [Dir1, Dir2],

          ( maplist(dif(Dir1-X1-Y1), Seen),
            run(Dir1-X1-Y1, MaxX-MaxY, Lines, MatSol, [Dir1-X1-Y1|Seen], Seen2)
          ; Seen2 = Seen
          ),
          ( maplist(dif(Dir2-X1-Y1), Seen),
            run(Dir2-X1-Y1, MaxX-MaxY, Lines, MatSol, [Dir1-X1-Y1|Seen2], Seen1)
          ; Seen1 = Seen2
          )
        )
    ;
      % outside of the matrix, in either dimension
      ( X1 in (inf..(-1))\/(MaxX..sup)
      ; Y1 in (inf..(-1))\/(MaxY..sup)
      ),
      Seen1 = Seen
    ).

solve(Dir-X-Y, Lines, Val) :-
    % create matrix of unknown (yet) variables of same size
    same_length(Lines, MatSol),
    maplist(same_length, Lines, MatSol),

    % get matrix bounds
    length(Lines, MaxY),
    Lines = [L|_],
    length(L, MaxX),

    % run: unify variables which the light crosses
    run(Dir-X-Y, MaxX-MaxY, Lines, MatSol, [], _),

    % any other unbound variable, set it to 0
    maplist(maplist(\Z^(var(Z), Z = 0; true)), MatSol),

    % sum the 1s
    append(MatSol, Sols),
    sum_list(Sols, Val).

% astonishing 60 seconds to run!
part1(File, Sol) :-
    phrase_from_file(lines(Lines), File),
    solve(right-(-1)-0, Lines, Sol).

% didn't finish it, it has to be trivially correct but wow!
part2(File, Sol) :-
    phrase_from_file(lines(Lines), File),

    % get bounds
    length(Lines, MaxY),
    Lines = [L|_],
    length(L, MaxX),

    % enumerate indices
    MaxX1 #= MaxX - 1,
    numlist(0, MaxX1, Xs1),

    % run the above starting on top and bottom of the matrix
    maplist([Lines,MaxY]+\X^Val^(
                format("X = ~d~n", [X]),
                time(solve(down-X-(-1), Lines, Val1)),
                time(solve(up-X-MaxY, Lines, Val2)),
                format("~d-0: ~d~n~d-Max: ~d~n", [X, Val1, X, Val2]),
                Val #= max(Val1, Val2)
            ),
            Xs1,
            Sols1),

    % enumerate indices
    MaxY1 #= MaxY - 1,
    numlist(0, MaxY1, Ys1),

    % run the above starting on left and right of the matrix
    maplist([Lines,MaxX]+\Y^Val^(
                format("Y = ~d~n", [Y]),
                time(solve(right-(-1)-Y, Lines, Val1)),
                time(solve(left-MaxX-Y, Lines, Val2)),
                format("0-~d: ~d~nMax-~d: ~d~n", [Y, Val1, Y, Val2]),
                Val #= max(Val1, Val2)
            ),
            Ys1,
            Sols2),

    % get the max of all lists
    append([Sols1, Sols2], Sols),
    list_max(Sols, Sol).
