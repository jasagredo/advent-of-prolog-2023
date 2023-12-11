:- use_module(utils).

sol1(54601).
sol2(54078).

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
  ;
  num(X), { Xs = [X|Xss] }, num/digit(Xss)
  ;
  [X], { numeric(X) }, num/digit(Xss), {Xs = [X|Xss]}
  ;
  [_], num/digit(Xs).

part1(F, Solution) :-
  phrase_from_file(lines(Lines), F),
  maplist(\L^Num^(filter(numeric, L, [D|Ds]),
                  reverse([D|Ds], [D2|_]),
                  number_chars(Num, [D, D2])
               ), Lines, Nums),
  sum_list(Nums, Solution).

part2(F, Solution) :-
  phrase_from_file(lines(Lines), F),
  maplist(\L^Num^(phrase(num/digit([X|Xs]), L),
                  reverse([X|Xs], [Y|_]),
                  number_chars(Num, [X, Y])
                 ), Lines, Nums),
  sum_list(Nums, Solution).
