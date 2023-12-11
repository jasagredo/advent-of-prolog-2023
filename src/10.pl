:- use_module(utils).

sol1(6806).
sol2(449).

next(X-Y, '-', r, X-Y1, r) :- Y1 #= Y + 1.
next(X-Y, '-', l, X-Y1, l) :- Y1 #= Y - 1.

next(X-Y, '|', d, X1-Y, d) :- X1 #= X + 1.
next(X-Y, '|', u, X1-Y, u) :- X1 #= X - 1.

next(X-Y, 'L', d, X-Y1, r) :- Y1 #= Y + 1.
next(X-Y, 'L', l, X1-Y, u) :- X1 #= X - 1.

next(X-Y, 'F', u, X-Y1, r) :- Y1 #= Y + 1.
next(X-Y, 'F', l, X1-Y, d) :- X1 #= X + 1.

next(X-Y, '7', u, X-Y1, l) :- Y1 #= Y - 1.
next(X-Y, '7', r, X1-Y, d) :- X1 #= X + 1.

next(X-Y, 'J', d, X-Y1, l) :- Y1 #= Y - 1.
next(X-Y, 'J', r, X1-Y, u) :- X1 #= X - 1.

next(X-Y, 'S', D, X1-Y1, D2) :-
    next(X-Y, '-', D, X1-Y1, D2)
    ; next(X-Y, '|', D, X1-Y1, D2)
    ; next(X-Y, 'L', D, X1-Y1, D2)
    ; next(X-Y, 'F', D, X1-Y1, D2)
    ; next(X-Y, '7', D, X1-Y1, D2)
    ; next(X-Y, 'J', D, X1-Y1, D2).

run(Mat, Positions, Sol) :-
    run_(Mat, 1, Positions, Sol).
run_(Mat, Idx, Positions, Sol) :-
    Idx1 #= Idx + 1,
    findall(U1-(V1-D1),
            (member(U-(V-D), Positions),
             mat_at(U-V, Mat, X),
             next(U-V, X, D, U1-V1, D1)
            ),
            NextPositions),
    ( permutation(NextPositions, [A-(B-_), A-(B-_)|_]),
      Sol = Idx1
    ; run_(Mat, Idx1, NextPositions, Sol)
    ).

part1(F, Sol) :-
    phrase_from_file(lines(Mat), F),

    % Find S
    append([Pre, [L], _], Mat),
    append([Pre1, "S", _], L),
    length(Pre, X),
    length(Pre1, Y),
    length(Mat, XMax),
    length(L, YMax),

    findall(U-(V-D), (next(X-Y, 'S', _, U-V, D), U #>= 0, U #<XMax, V #>=0, V #<YMax), Starts0),
    list_to_set(Starts0, Starts),

    run(Mat, Starts, Sol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

part2(F, Res) :-
    phrase_from_file(lines(Mat), F),
    append([Pre, [L], Post], Mat),
    append([Pre1, "S", Post1], L),
    length(Pre, X),
    length(Pre1, Y),
    length(Mat, XMax),
    length(L, YMax),

    findall(U-(V-D), (next(X-Y, 'S', _, U-V, D),
                      U #>= 0,
                      U #<XMax,
                      V #>= 0,
                      V #<YMax
                     ), Starts0),
    list_to_set(Starts0, Starts),

    run2(Mat, Starts, Sol),

    % Replace the "S" (I manually checked this, perhaps I could have computed it)
    append([Pre1, "J", Post1], L1),
    append([Pre, [L1], Post], Mat1),

    % Perhaps this is cheating, I remove the two starts that I don't want.
    Y1 #= Y + 1,
    select(X-(Y1-_), Starts, Starts1),
    X1 #= X + 1,
    select(X1-(Y-_), Starts1, Starts2),

    append([[X-(Y-dUMMY)|Starts2], Sol], Sol1),
    maplist(\ (U-(V-_))^(U-V)^true, Sol1, Sol2),

    % Point in polygon by lines
    pip(Mat1, 0, Sol2, Res).

% same as run but accumulates the solutions instead of just returning a number
run2(Mat, Positions, Sol) :-
    run2_(Mat, 1, Positions, Sol).
run2_(Mat, Idx, Positions, Sol) :-
    Idx1 #= Idx + 1,
    findall(U1-(V1-D1),
            (member(U-(V-D), Positions),
             mat_at(U-V, Mat, X),
             next(U-V, X, D, U1-V1, D1)
            ),
            NextPositions),
    ( NextPositions = [A-(B-_), A-(B-_)],
      Sol = [A-(B-dUMMY)]
    ; run2_(Mat, Idx1, NextPositions, NextNext),
      append(NextPositions, NextNext, Sol)
    ).

% Generate the structs. The resulting list contains:
%
% - X-bar = a vertical bar at position X
%
% - X-(Y-cross) = a crossing line from X to Y
%
% - X-(Y-nocross) = a noncrossing line from X to Y
%
structs(_, [], []).
structs(Line, [X|More], [X-bar|Next] ) :-
    nth0(X, Line, '|'),
    structs(Line, More, Next).
structs(Line, [X, Y|More], Next) :-
    nth0(X, Line, 'F'),
    nth0(Y, Line, '-'),
    structs(Line, [X|More], Next).
structs(Line, [X, Y|More], Next) :-
    nth0(X, Line, 'L'),
    nth0(Y, Line, '-'),
    structs(Line, [X|More], Next).
structs(Line, [X, Y|More], [X-(Y-cross)|Next]) :-
    nth0(X, Line, 'L'),
    nth0(Y, Line, '7'),
    structs(Line, More, Next).
structs(Line, [X, Y|More], [X-(Y-nocross)|Next]) :-
    nth0(X, Line, 'L'),
    nth0(Y, Line, 'J'),
    structs(Line, More, Next).
structs(Line, [X, Y|More], [X-(Y-nocross)|Next]) :-
    nth0(X, Line, 'F'),
    nth0(Y, Line, '7'),
    structs(Line, More, Next).
structs(Line, [X, Y|More], [X-(Y-cross)|Next]) :-
    nth0(X, Line, 'F'),
    nth0(Y, Line, 'J'),
    structs(Line, More, Next).

% Process the structs accumulating the number of ON tiles
procStructs(_, [], 0).
procStructs(_, [_], 0).

procStructs(off, [X-bar, Y-Sth|More], This) :-
    procStructs(on, [Y-Sth|More], Next),
    This #= Y - X - 1 + Next.
procStructs(on, [_-bar, Y-Sth|More], Next) :-
    procStructs(off, [Y-Sth|More], Next).

procStructs(off, [_-(Y-cross), Z-Sth|More], This) :-
    procStructs(on, [Z-Sth|More], Next),
    This #= Z - Y - 1 + Next.
procStructs(on, [_-(_-cross), Z-Sth|More], Next) :-
    procStructs(off, [Z-Sth|More], Next).

procStructs(off, [_-(_-nocross), Z-Sth|More], Next) :-
    procStructs(off, [Z-Sth|More], Next).
procStructs(on, [_-(Y-nocross), Z-Sth|More], This) :-
    procStructs(on, [Z-Sth|More], Next),
    This #= Z - Y - 1 + Next.

% Ray casting for Point in Polygon
pip(Mat, N, _, 0) :- length(Mat, N).
pip(Mat, N, Graph, Sol) :-
    % get the edges in this line
    filter( N +\ (N-_)^true, Graph, ThisLine ),
    % get the Ys in this line
    maplist( \ (_-Y)^Y^true, ThisLine, ThisLine1),
    % sort them
    list_to_ord_set(ThisLine1, Sorted),

    % get this line of matrix
    nth0(N, Mat, Line),

    % get the structs in this line
    structs(Line, Sorted, Structs),

    % sum as many tiles as are ON
    procStructs(off, Structs, This),

    N1 #= N + 1,
    pip(Mat, N1, Graph, Nx),
    Sol #= Nx + This.
