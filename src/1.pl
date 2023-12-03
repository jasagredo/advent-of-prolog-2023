:- use_module(utils).

run :-
  format("Solving day 1~n", []),
  time(solve1('inputs/1.txt', Sol1)),
  format("Solution 1: ~q~n", [Sol1]),
  time(solve2('inputs/1.txt', Sol2)),
  format("Solution 2: ~q~n", [Sol2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    DCGs                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Data parsing DCGs
first_digit(X) --> [Z], { char_type(Z, alpha) }, first_digit(X).
first_digit(X) --> [X], { char_type(X, numeric) }, ... .

num('1') --> "one".
num('2') --> "two".
num('3') --> "three".
num('4') --> "four".
num('5') --> "five".
num('6') --> "six".
num('7') --> "seven".
num('8') --> "eight".
num('9') --> "nine".

first_num_or_digit(X) -->
  ...,
  ( [X], { char_type(X, numeric) }
  ; num(X) ),
  ... .
first_num_or_digit_r(X) -->
  ...,
  ( [X], { char_type(X, numeric) }
  ; seq(Z), { reverse(Z, Z1), phrase(num(X), Z1)} ),
  ... .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   Part 1                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve1(F, Res) :-
  phrase_from_file(lines(Lines), F),
  maplist(\L^Ls^(
                 phrase(first_digit(X), L),
                 reverse(L, L1),
                 phrase(first_digit(Y), L1),
                 number_chars(Ls, [X, Y])
                ), Lines, Lines2),
  sum_list(Lines2, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   Part 2                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(F, Res) :-
  phrase_from_file(lines(Lines), F),
  maplist(\L^Ls^(
                 phrase(first_num_or_digit(X), L),
                 reverse(L, L1),
                 phrase(first_num_or_digit_r(Y), L1),
                 number_chars(Ls, [X, Y])
                ), Lines, Lines2),
  sum_list(Lines2, Res).
