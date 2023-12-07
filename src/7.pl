:- use_module(assoc_by).
:- use_module(library(assoc)).
:- use_module(utils).

run :-
  format("Solving day 7~n", []),
  format("Part 1: ~n", []),
  time(part1('inputs/7.txt', Sol1)),
  format("   ", []), isok(Sol1, 248812215), !,
  format("Part 2: ~n", []),
  time(part2('inputs/7.txt', Sol2)),
  format("   ", []), isok(Sol2, 250057090).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compareChar(=, X, X).
compareChar(<, _, 'A').
compareChar(<, X, 'K') :-
    X = 'Q';
    compareChar(<, X, 'Q').
compareChar(<, X, 'Q') :-
    X = 'J';
    compareChar(<, X, 'J').
compareChar(<, X, 'J') :-
    X = 'T';
    compareChar(<, X, 'T').
compareChar(<, X, 'T') :-
    char_type(X, numeric).
compareChar(R, X, Y) :-
    char_type(X, numeric),
    char_type(Y, numeric),
    number_chars(NX, [X]),
    number_chars(NY, [Y]),
    compare(R, NX, NY).
compareChar(>, X, Y) :- compareChar(<, Y, X).

compareList(R, [L|Ls], [M|Ms]) :-
    compareChar(Rel, L, M),
    ( Rel = (=),
      compareList(R, Ls, Ms)
    ; R = Rel
    ).

compareType(=, X, X).
compareType(<, _, repoker).
compareType(<, X, poker) :-
    X = house;
    compareType(<, X, house).
compareType(<, X, house) :-
    X = three;
    compareType(<, X, three).
compareType(<, X, three) :-
    X = two-pair;
    compareType(<, X, two-pair).
compareType(<, X, two-pair) :-
    X = pair;
    compareType(<, X, pair).
compareType(<, high, pair).
compareType(>, X, Y) :- compareType(<, Y, X).

compareHand(R, X, U) :-
    type(X, Z), type(U, W), compareType(Rel, Z, W),
    ( Rel = (=),
      compareList(R, X, U)
    ; Rel = R
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count(A, L, N) :-
    findall(_, member(A, L), Z),
    length(Z, N).

type(L, repoker) :-
    maplist((=(_)), L).
type(L, poker) :-
    setof(X, member(X, L), L1),
    maplist(\ X^N^(count(X, L, N)), L1, L2),
    permutation(L2, [1,_]).
type(L, house) :-
    setof(X, member(X, L), [_, _]).
type(L, three) :-
    setof(X, member(X, L), L1),
    maplist(\ X^N^(count(X, L, N)), L1, L2),
    permutation(L2, [1,1,_]).
type(L, two-pair) :-
    setof(X, member(X, L), [_, _, _]).
type(L, pair) :-
    setof(X, member(X, L), [_, _, _, _]).
type(_, high).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

part1(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    empty_assoc(Assoc),
    foldl(\ L^Assoc0^Assoc1^(
              phrase(seqDelimited(' ', word, [Key, V]), L),
              number_chars(Val, V),
              put_assoc_by(compareHand, Key, Assoc0, Val, Assoc1)
          ), Lines, Assoc, AssocRes),
    assoc_to_list(AssocRes, Sorted),
    foldl(\ (_-V)^(Idx-Acc)^(Idx1-Acc1)^(
              Idx1 #= Idx + 1,
              Acc1 #= Acc + (V * Idx)
          ),
          Sorted,
          1-0,
          _-Sol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compareChar2(=, X, X).
compareChar2(<, 'J', _).
compareChar2(>, _, 'J').
compareChar2(R, X, Y) :- compareChar(R, X, Y).

compareList2(R, [L|Ls], [M|Ms]) :-
    compareChar2(Rel, L, M),
    ( Rel = (=),
      compareList2(R, Ls, Ms)
    ; R = Rel
    ).

compareHand2(R, X, U) :-
    type2(X, Z),
    type2(U, W),
    compareType(Rel, Z, W),
    ( Rel = (=),
      compareList2(R, X, U)
    ; Rel = R
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type2(L, T) :-
    filter(\X^(\+ X = 'J'), L, L1),
    type(L1, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

part2(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    empty_assoc(Assoc),
    foldl(\ L^Assoc0^Assoc1^(
              phrase(seqDelimited(' ', word, [Key, V]), L),
              number_chars(Val, V),
              put_assoc_by(compareHand2, Key, Assoc0, Val, Assoc1)),
          Lines,
          Assoc,
          AssocRes),
    assoc_to_list(AssocRes, Sorted),
    foldl(\ (_-V)^(Idx-Acc)^(Idx1-Acc1)^(
              Idx1 #= Idx + 1,
              Acc1 #= Acc + (V * Idx)
          ),
          Sorted,
          1-0,
          _-Sol).
