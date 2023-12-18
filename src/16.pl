:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(between)).
:- use_module(library(debug)).

matrix(_, MaxY, _, MaxY, A, A) --> [].
matrix(MaxX, Y, MaxX, MaxY, A0, A1) -->
    { Y1 #= Y + 1 },
    "\n",
    matrix(0, Y1, MaxX, MaxY, A0, A1).
matrix(X, Y, MaxX, MaxY, A0, A1) -->
    { X #>= 0, X #< MaxX, Y #>= 0, Y #< MaxY },
    [Char],
    { put_assoc(X-Y, A0, Char, A2), X1 #= X + 1 },
    matrix(X1, Y, MaxX, MaxY, A2, A1).

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

run(Dir-X-Y, MaxX-MaxY, Lines, Seen, Seen1) :-
    next_position(Dir, X-Y, X1-Y1),
    Xz #= MaxX - 1,
    Yz #= MaxY - 1,
    (
        % when inside the matrix
        X1 in 0..Xz,
        Y1 in 0..Yz,

        % get next step
        get_assoc(X1-Y1, Lines, E),
        reflect(Dir, E, Next),

        (
            % only one next direction
            Next = [Dir1],
            (
                % if we have seen it, do nothing
                get_assoc(Dir1-X1-Y1, Seen, _),
                Seen1 = Seen
            ;
                % if we have not seen it yet
                put_assoc(Dir1-X1-Y1, Seen, yes, SeenAux),
                run(Dir1-X1-Y1, MaxX-MaxY, Lines, SeenAux, Seen1)
            )
        ;
          % two directions (a splitter)
          Next = [Dir1, Dir2],

          (
              % if we have seen it, do nothing
              get_assoc(Dir1-X1-Y1, Seen, _),
              Seen2 = Seen
          ;
              % if we have not seen it yet
              put_assoc(Dir1-X1-Y1, Seen, yes, SeenAux1),
              run(Dir1-X1-Y1, MaxX-MaxY, Lines, SeenAux1, Seen2)
          ),
          (
              % if we have seen it, do nothing
              get_assoc(Dir2-X1-Y1, Seen2, _),
              Seen1 = Seen2
          ;
              % if we have seen it, do nothing
              put_assoc(Dir2-X1-Y1, Seen2, yes, SeenAux2),
              run(Dir2-X1-Y1, MaxX-MaxY, Lines, SeenAux2, Seen1)
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
    empty_assoc(A),
    run(Dir-X-Y, 110-110, Lines, A, Seen),
    assoc_to_keys(Seen, Seen2), !,
    maplist(\ (_-B-C)^(B-C)^true, Seen2, Seen3),
    sort(Seen3, Seen4),
    length(Seen4, Val).

part1(File, Sol) :-
    empty_assoc(A),
    phrase_from_file(matrix(0, 0, 110, 110, A, Lines), File), !,
    solve(right-(-1)-0, Lines, Sol).

% didn't finish it, it has to be trivially correct but wow!
part2(File, Sol) :-
    empty_assoc(A),
    phrase_from_file(matrix(0, 0, 110, 110, A, Lines), File),

    numlist(0, 109, Xs1),

    % run the above starting on top and bottom of the matrix
    maplist(\X^Val^(
                solve(down-X-(-1), Lines, Val1),
                solve(up-X-110, Lines, Val2),
                Val #= max(Val1, Val2)
            ),
            Xs1,
            Sols1),

    % enumerate indices
    numlist(0, 109, Ys1),

    % run the above starting on left and right of the matrix
    maplist(\Y^Val^(
                solve(right-(-1)-Y, Lines, Val1),
                solve(left-110-Y, Lines, Val2),
                Val #= max(Val1, Val2)
            ),
            Ys1,
            Sols2),

    % get the max of all lists
    append([Sols1, Sols2], Sols),
    list_max(Sols, Sol).
