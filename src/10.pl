:- use_module(utils).

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

part1(Sol) :-
    phrase_from_file(lines(Mat), 'inputs/10.txt'),

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

% Jesus take the wheel

% ignore everything that follows, I will come back and fix it

part2(Res) :-
    phrase_from_file(lines(Mat), 'inputs/10.txt'),
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

    write(Sol), nl,
    append([Pre1, "7", Post1], L1),
    append([Pre, [L1], Post], Mat1),

    append([[X-(Y-dUMMY)|Starts], Sol], Sol1),
    maplist(\ (U-(V-_))^(U-V)^true, Sol1, Sol2),
    !,
    pip(Mat1, 0, Sol2, Res).

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

qes([X|Xs]) --> [X], qes(Xs).
qes([]) --> [].

count([], 0).
count([Y, W|Nx], Count) :-
    count(Nx, Count1),
    Count #= Count1 + (W - Y - 1).

all_dash(Mat, N, [X|Xs]) :-
    mat_at(N-X, Mat, '-'),
    all_dash(Mat, N, Xs).
all_dash(_, _, []).

crossing(Mat, N, Crossing) -->
    [X],
    { mat_at(N-X, Mat, 'L') },
    qes(S),
    {  all_dash(Mat, N, S) },
    [Y],
    { mat_at(N-Y, Mat, 'J') },
    crossing(Mat, N, Crossing).
crossing(Mat, N, [X,Y|Crossing]) -->
    [X],
    { mat_at(N-X, Mat, 'L') },
    qes(S),
    { all_dash(Mat, N, S) },
    [Y],
    { mat_at(N-Y, Mat, '7') },
    crossing(Mat, N, Crossing).
crossing(Mat, N, [X, Y|Crossing]) -->
    [X],
    { mat_at(N-X, Mat, 'F')},
    qes(S),
    { all_dash(Mat, N, S) },
    [Y],
    { mat_at(N-Y, Mat, 'J') },
    crossing(Mat, N, Crossing).
crossing(Mat, N, Crossing) -->
    [X],
    { mat_at(N-X, Mat, 'F')},
    qes(S),
    {  all_dash(Mat, N, S) },
    [Y],
    { mat_at(N-Y, Mat, '7') },
    crossing(Mat, N, Crossing).
crossing(Mat, N, [X|Crossing]) -->
    [X],
    { mat_at(N-X, Mat, '|') },
    crossing(Mat, N, Crossing).
crossing(Mat, N, Crossing) --> [_], crossing(Mat, N, Crossing).
crossing(_, _, []) --> [].



w(off, '|', _, on).
w(off, 'F', _, on).

foo('|', Yy, X, Y, This) :-
    This #= Y - X - 1.
foo(Xx, '|', X, Y, This) :-
    This #= Y - X - 1.
foo('J', 'F', X, Y, This) :-
    This #= Y - X - 1.
foo('7', 'L', X, Y, This) :-
    This #= Y - X - 1.
foo(_, _, _, _, 0).

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

procStructs(_, [], 0).
procStructs(_, [_], 0).
procStructs(off, [X-bar, Y-Sth|More], This) :-
    procStructs(on, [Y-Sth|More], Next),
    This #= Y - X - 1 + Next,
    (This #> Next, format("~d-bar, adds to ~d~n", [X, This]); true).
procStructs(on, [X-bar, Y-Sth|More], Next) :-
    procStructs(off, [Y-Sth|More], Next).

procStructs(off, [X-(Y-cross), Z-Sth|More], This) :-
    procStructs(on, [Z-Sth|More], Next),
    This #= Z - Y - 1 + Next,

    (This #> Next, format("~d-~d-cross, adds to ~d~n", [X, Y, This]); true).
procStructs(on, [X-(Y-cross), Z-Sth|More], Next) :-
    procStructs(off, [Z-Sth|More], Next).

procStructs(off, [X-(Y-nocross), Z-Sth|More], Next) :-
    procStructs(off, [Z-Sth|More], Next).
procStructs(on, [X-(Y-nocross), Z-Sth|More], This) :-
    procStructs(on, [Z-Sth|More], Next),
    This #= Z - Y - 1 + Next,

    (This #> Next, format("~d-~d-nocross, adds to ~d~n", [X, Y, This]); true).

count2(_, _, [], []).
count2(_, _, [_], []).
count2(W, Line, [X, Y|More], Sol) :-
    nth0(X, Line, Xx),
    nth0(Y, Line, Yy),
    w(W, Xx, Yy, NW),
    foo(Xx, Yy, X, Y, This),
    %% ( ( Xx = '|' ; Yy = '|'), This #= (Y - X) - 1
    %% ; This #= 0
    %% ),
    count2(Line, [Y|More], NextSol),
    ( This #= 0, Sol = NextSol
    ; Sol = [(X-Y)-This|NextSol] ).

% Ray tracing for Point in Polygon
pip(_, 140, _, 0).
pip(Mat, N, Graph, Sol) :-
    % get the edges in this line
    filter( N +\ (N-_)^true, Graph, ThisLine ),
    % get the Ys in this line
    maplist( \ (_-Y)^Y^true, ThisLine, ThisLine1),
    % sort them
    list_to_ord_set(ThisLine1, Sorted),

    nth0(N, Mat, Line),
    structs(Line, Sorted, Structs),
    procStructs(off, Structs, This),
    write([N, Structs, This]), nl,
    %% count2(off, Line, Sorted, Count2),
    %% write(Count2), nl,

    N1 #= N + 1,
    pip(Mat, N1, Graph, Nx),
    Sol #= Nx + This.

    %% Sol #= SolNx + Count2
