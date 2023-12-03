:- use_module(utils).

run :-
  format("Solving day 2~n", []),
  time(solve1('inputs/2.txt', Sol1)),
  format("Solution 1: ~q~n", [Sol1]),
  time(solve2('inputs/2.txt', Sol2)),
  format("Solution 2: ~q~n", [Sol2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    DCGs                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Data parsing DCGs
game(g(Id, Plays)) -->
  "Game ", seq(Id0), { number_string(Id, Id0) },  ": ", !,
  plays(Plays).

plays([])     --> ("\n" ; call(eos)), ! .
plays([P|Ps]) --> play(P), ";", plays(Ps).

play([G|Gs])   --> a_color(G), ", ", !, play(Gs).
play([G]), ";" --> a_color(G), ( ";" ; call(eos)).

a_color(c(N, C)) --> seq(N0), { number_string(N, N0) }, " ", color(C).

color(r) --> "red" .
color(b) --> "blue" .
color(g) --> "green" .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   Part 1                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_color_under(Ls, C, S) :- member(c(X, C), Ls), X #=< S.
is_color_under(Ls, C, _) :- \+ member(c(_, C), Ls).

all_colors_under(L) :-
  forlist([r,g,b], is_color_under(L), [12, 13, 14]).

solve1(F, Sol) :-
  phrase_from_file(lines(Lines), F),
  maplist(\Line^Game^phrase(game(Game), Line), Lines, Games),
  filter(\g(_, Plays)^maplist(all_colors_under, Plays), Games, OkGames),
  maplist(\g(I, _)^I^true, OkGames, Ids),
  sum_list(Ids, Sol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   Part 2                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_power(Plays, Pow) :-
  forlist(
          [r,g,b],
          \C^Max^(
                  maplist(C+\P^R^(
                                  member(c(R, C), P);
                                  \+ member(c(_, C), P), R #= 0
                                 ), Plays, ThisColor),
                  list_max(ThisColor, Max)),
          [MaxRed, MaxGreen, MaxBlue]),
  Pow #= MaxRed * MaxGreen * MaxBlue .

solve2(F, Sol) :-
  phrase_from_file(lines(Lines), F),
  maplist(\Line^Pow^(
                     phrase(game(g(_, Plays)), Line),
                     compute_power(Plays, Pow))
         , Lines, Powers),
  sum_list(Powers, Sol).
