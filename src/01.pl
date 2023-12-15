:- use_module(library(dcgs)).
:- use_module(dcgs/utils).

sol1(54601).
sol2(54078).

% Due to limitations on DCGs, we cannot say the following:
%
% ```
% blah, L --> L, ...
% ```
%
% where L must be a list as it is part of the body. This function emulates what
% I wanted to write instead.
f(L,C,D) :-
  phrase(seq(L), C, E),
  L = [_|Ls],
  append(Ls, E, D).

num('1') --> f("one").
num('2') --> f("two").
num('3') --> f("three").
num('4') --> f("four").
num('5') --> f("five").
num('6') --> f("six").
num('7') --> f("seven").
num('8') --> f("eight").
num('9') --> f("nine").

num/digit(Xs) -->
    call(eos), {Xs = []}
  |
    num(X), { Xs = [X|Xss] }, num/digit(Xss)
  |
    [X], { char_type(X, numeric) }, num/digit(Xss), {Xs = [X|Xss]}
  |
    [_], num/digit(Xs).

flip(F, X, Y) :-
  call(F, Y, X).

improved([X,Y]) -->
  eager_seq_of(flip(char_type, alpha), _),
  [X], { char_type(X, numeric) },
  ...,
  [Y], { char_type(Y, numeric) },
  eager_seq_of(flip(char_type, alpha), _).
improved([X]) -->
  ..., [X], { char_type(X, numeric) }, ... .

part1(F, Solution) :-
  phrase_from_file(lines(Lines), F),
  maplist(\L^Num^(phrase(improved(X), L),
                  ( X = [_, _], Y = X
                  ; X = [A], Y = [A, A]),
                  number_chars(Num, Y)
                 ),
          Lines,
          Nums),
  sum_list(Nums, Solution).

part2(F, Solution) :-
  phrase_from_file(lines(Lines), F),
  maplist(\L^Num^(phrase(num/digit([X|Xs]), L),
                  reverse([X|Xs], [Y|_]),
                  number_chars(Num, [X, Y])
                 ),
          Lines,
          Nums),
  sum_list(Nums, Solution).
