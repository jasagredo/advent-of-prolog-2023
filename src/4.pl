:- use_module(utils).

run :-
  format("Solving day 4~n", []),
  time(solve1('inputs/4.txt', Sol1)),
  format("Solution 1: ~q~n", [Sol1]),
  time(solve2('inputs/4.txt', Sol2)),
  format("Solution 2: ~q~n", [Sol2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 DCGs                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Must start with a number, so parse the spaces before calling this.
numbers([]), "|" --> "|", !.
numbers([])      --> ( "\n" ; call(eos) ), !.
numbers([N|R])   --> seqof(numeric, X),
                     { number_string(N, X) },
                     seqof_(whitespace),
                     numbers(R).

games([])           --> call(eos), !.
games([Line|Lines]) --> game(Line), games(Lines).

game(L) -->
    "Card", seqof_(whitespace), seqof_(numeric), ":", seqof_(whitespace),
    numbers(N), "|", seqof_(whitespace), numbers(M),
    { findall(X, (member(X, N), member(X, M)), Matches),
      length(Matches, L) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Part 1                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve1(File, Sol) :-
    phrase_from_file(games(Games), File),
    maplist(\L^Sol^( L = 0, Sol #= 0
                   ; Sol #= 2 ^ (L - 1)),
            Games,
            Games1),
    sum_list(Games1, Sol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Part 2                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(File, Sol) :-
    phrase_from_file(games(Games), File),
    same_length(Games, CardCounts),
    maplist(\1^true, CardCounts),
    foldl(accumulate, Games, (0, CardCounts), (Sol, _)).

accumulate(GameResult, (AccCards, [ThisCard|NextCards]), (AccCards2, NextCards2)) :-
    AccCards2 #= AccCards + ThisCard,
    append(Duplicating, Unchanging, NextCards),
    length(Duplicating, GameResult),
    maplist(\X^Y^(Y #= X + ThisCard), Duplicating, Duplicating2),
    append(Duplicating2, Unchanging, NextCards2).
