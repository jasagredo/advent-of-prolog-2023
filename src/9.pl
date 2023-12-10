:- use_module(utils).

diff_values([_], []).
diff_values([X, Y|Z], [XY|YZ]) :-
    XY #= Y - X,
    diff_values([Y|Z], YZ).

diff_until_zeroes(Xs, [X|XXs]) :-
    diff_values(Xs, X),
    ( maplist((#=(0)), X), XXs = []
    ; diff_until_zeroes(X, XXs)
    ).

solve(Xs, Sol) :-
    diff_until_zeroes(Xs, Diff),
    reverse([Xs|Diff], Diff1),
    maplist(reverse, Diff1, [_|Diff2]),
    maplist(\ ([X|_])^X^true, Diff2, Diff3),
    sum_list(Diff3, Sol).

part1(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    maplist(\X^Y^phrase(seqDelimited(' ', znumber, Y), X),
            Lines,
            Ls),
    maplist(solve, Ls, Ys),
    sum_list(Ys, Sol).

diff_values(X, [], X).
diff_values(X, [V|Vs], Sol) :-
    X #= V - Y,
    diff_values(Y, Vs, Sol).

solve2(Xs, Sol) :-
    diff_until_zeroes(Xs, Diff),
    reverse([Xs|Diff], Diff2),
    maplist(\ ([X|_])^X^true, Diff2, Diff3),
    diff_values(0, Diff3, Sol).

part2(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    maplist(\X^Y^phrase(seqDelimited(' ', znumber, Y), X),
            Lines,
            Ls),
    maplist(solve2, Ls, Ys),
    sum_list(Ys, Sol).
