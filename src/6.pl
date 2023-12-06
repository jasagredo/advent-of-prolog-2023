:- use_module(utils).

run :-
  format("Solving day 6~n", []),
  format("Part 1: ~n", []),
  time(part1('inputs/6.txt', Sol1)),
  format("   ", []), isok(Sol1, 303600), !,
  format("Part 2: ~n", []),
  time(part2('inputs/6.txt', Sol2)),
  format("   ", []), isok(Sol2, 23654842).

parse1(Times, Distances) -->
    "Time:", seqof_(whitespace), seqDelimited(' ', number, Times), "\n",
    "Distance:", seqof_(whitespace), seqDelimited(' ', number, Distances), "\n".

solveGame(Time, Distance, Solution) :-
    findall(X, (X #> 0,
                X #< Time,
                (Time - X)*X #> Distance,
                label([X])), Winners),
    length(Winners, Solution).

part1(F, Sol) :-
    phrase_from_file(parse1(Times, Distances), F),
    maplist(solveGame, Times, Distances, Solutions),

    % multiply_list
    foldl(\X^Y^Z^(Z #= X * Y), Solutions, 1, Sol).

parse2(Time, Distance) -->
    "Time:", seqof_(whitespace), seqDelimited(' ', a_digit, Times), "\n",
    "Distance:", seqof_(whitespace), seqDelimited(' ', a_digit, Distances), "\n",
    { number_chars(Time, Times), number_chars(Distance, Distances) }.

part2(F, Solution) :-
    phrase_from_file(parse2(Time, Distance), F),
    % find lower bound
    X #> 0,
    X #< Time,
    (Time - X)*X #> Distance,
    (Time - (X - 1)) * (X - 1) #=< Distance,
    labeling([bisect], [X]),

    % find upper bound
    Y #> 0,
    Y #< Time,
    (Time - Y)*Y #=< Distance,
    (Time - (Y - 1)) * (Y - 1) #> Distance,
    labeling([bisect], [Y]),

    Solution #= Y - X.
