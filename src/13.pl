:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).

mirrors_([], []).
mirrors_([_|_], []).
mirrors_([], [_|_]).
mirrors_([X|A], [X|B]) :- mirrors_(A, B).
mirrors(A, B) :-
    A = [_|_], B = [_|_],
    reverse(A, A1), mirrors_(A1, B).

matrix_mirror(Mat, X, Y) :-
    $ mirrors(X, Y),
    append(X, Y, Mat).

matrix_to_value(Mat, Val) :-
      matrix_mirror(Mat, X, _),
      length(X, Val1),
      Val #= Val1 * 100
    ; transpose(Mat, Mat1),
      matrix_mirror(Mat1, X, _),
      length(X, Val).

a_line([X|Xs]) --> [X], { char_type(X, ascii_graphic) }, a_line(Xs).
a_line([]), "\n" --> "\n".

a_block([]) --> ("\n"; call(\ []^[]^true)).
a_block([L|Ls]) --> a_line(L), "\n", a_block(Ls).

blocks_of_lines([]) --> call(\ []^[]^true).
blocks_of_lines([G|Gs]) -->
    a_block(G), blocks_of_lines(Gs).

part1(F, Solution) :-
    phrase_from_file(blocks_of_lines(Lines), F),
    maplist(matrix_to_value, Lines, Values),
    sum_list(Values, Solution).

diff_1(tt, [], []).
diff_1(ff, [X|Xs], [Y|Ys]) :- dif(X, Y), diff_1(tt, Xs, Ys).
diff_1(T, [X|Xs], [X|Ys]) :- diff_1(T, Xs, Ys).

smudge_(found, [], []).
smudge_(found, [_|_], []).
smudge_(found, [], [_|_]).
smudge_(found, [X|A], [X|B]) :- smudge_(found, A, B).
smudge_(notfound, [X|A], [X|B]) :- smudge_(notfound, A, B).
smudge_(notfound, [X|A], [Y|B]) :- diff_1(ff, X, Y), smudge_(found, A, B).
smudge(A, B) :-
    A = [_|_], B = [_|_],
    reverse(A, A1), smudge_(notfound, A1, B).

matrix_smudge(Mat, X, Y) :-
    append(X, Y, Mat),
    smudge(X, Y).

matrix_to_smudge(Mat, Val) :-
      matrix_smudge(Mat, X, _),
      length(X, Val1),
      Val #= Val1 * 100
    ; transpose(Mat, Mat1),
      matrix_smudge(Mat1, X, _),
      length(X, Val).

part2(F, Solution) :-
    phrase_from_file(blocks_of_lines(Lines), F),
    maplist(matrix_to_smudge, Lines, Values),
    sum_list(Values, Solution).
