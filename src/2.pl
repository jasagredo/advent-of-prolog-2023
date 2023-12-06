:- use_module(utils).

run :-
  format("Solving day 2~n", []),
  format("Part 1: ~n", []),
  time(part1('inputs/2.txt', Sol1)),
  format("   ", []), isok(Sol1, 1734),
  format("Part 2: ~n", []),
  time(part2('inputs/2.txt', Sol2)),
  format("   ", []), isok(Sol2, 70387).

game(Id-Plays) -->
  "Game ", number(Id),  ": " ,
  seqDelimited(';', seqDelimited(',', a_color), Plays0),
  { maplist(list_to_assoc, Plays0, Plays) }.

a_color(C-N) --> number(N), " ", color(C).

color(r) --> "red" .
color(b) --> "blue" .
color(g) --> "green" .

feasible(Query, A) :-
  map_assoc_with_key(\K^X^(get_assoc(K, Query, S), X #=< S), A).

part1(F, Sol) :-
  phrase_from_file(lines(Lines), F),
  maplist(\Line^Game^phrase(game(Game), Line), Lines, Games),
  list_to_assoc([r-12, g-13, b-14], Query),
  maplist(\G^X^( G = (Id-Plays),
                 maplist(feasible(Query), Plays),
                 X #= Id
               ; X #= 0
               ), Games, Ids),
  sum_list(Ids, Sol).

%% Part 2

compute_power(Plays, Pow) :-
  forlist([r,g,b],
          \C^Max^(
                  maplist(C+\P^R^(
                                  get_assoc(C, P, R);
                                  R #= 0
                          ),
                          Plays,
                          ThisColor),
                  list_max(ThisColor, Max)),
          [MaxRed, MaxGreen, MaxBlue]),
  Pow #= MaxRed * MaxGreen * MaxBlue.

part2(F, Sol) :-
  phrase_from_file(lines(Lines), F),
  maplist(\Line^Pow^(
                     phrase(game(_-Plays), Line),
                     compute_power(Plays, Pow))
         , Lines, Powers),
  sum_list(Powers, Sol).
