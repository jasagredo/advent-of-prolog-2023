:- use_module(library(reif)).
:- use_module(utils).

lane([]) --> call(eos).
lane([]), "#" --> "#".
lane([X|Ls]) -->
    [X], { X = '.'; X = 'O' }, lane(Ls).

eager_seq(G_1, [E|Es]) --> [E], { call(G_1, E) }, eager_seq(G_1, Es).
eager_seq(_, []) --> [].

tilt_lane([], []).
tilt_lane(Line, R) :-
    tfilter(=('.'), Line, L1),
    tfilter(=('O'), Line, L2),
    append(L1, L2, R).

tilt_line([]) --> call(eos).
tilt_line(R) --> lane(R1), { tilt_lane(R1, R) }, call(eos).
tilt_line(R2) -->
    lane(R1), { tilt_lane(R1, R) },
    eager_seq(=(#), Hashes),
    tilt_line(Rs),
    { append([R, Hashes, Rs], R2) }.

tilt_dish(X, Y) :-
    maplist(\L^Z^phrase(tilt_line(Z), L), X, Y).

dish_load(Dish, Load) :-
    maplist(\L^X^(findall(N, nth1(N, L, 'O'), Xs), sum_list(Xs, X)),
            Dish,
            Loads),
    sum_list(Loads, Load).

rotate_clockwise(X, Y) :-
    same_length(X, Y),
    maplist(same_length, X, Y),
    transpose(X, Z), maplist(reverse, Z, Y).

part1(File, Sol) :-
    phrase_from_file(lines(Dish), File),
    rotate_clockwise(Dish, Dish1),
    tilt_dish(Dish1, Dish2),
    dish_load(Dish2, Sol).

tilt_then_rotate(X, Y) :-
    tilt_dish(X, Z), rotate_clockwise(Z, Y).

dish_cycle(X, Y) :-
    repeat(4, tilt_then_rotate, X, Y).

repeat(N, G_2, X, Y) :-
    repeat_(N, [], G_2, X, Y).
repeat_(0, _, _, X, X) .
repeat_(N, Seen, G_2, X, Y) :-
    N #> 0,
    call(G_2, X, Z),
    ( nth1(Nth, Seen, Z),
      Rem #= (N rem Nth),
      repeat_(Rem, no, G_2, X, Y)
    ; N1 #= N - 1,
      repeat_(N1, [Z|Seen], G_2, Z, Y)
    ).

part2(File, Sol) :-
    phrase_from_file(lines(Dish), File),
    rotate_clockwise(Dish, Dish1),
    repeat(1000000000, dish_cycle, Dish1, DishN),
    dish_load(DishN, Sol).
