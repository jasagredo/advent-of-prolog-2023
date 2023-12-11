:- use_module(utils).

sol1(424490994).
sol2(15290096).

maps --> call(eos).
maps --> "\n",
         seqof(alpha, From),
         "-to-",
         seqof(alpha, To),
         " map:\n",
         maprelations(From, To),
         { assertz(rel(From, To, X, X)) },
         maps.

maprelations(From, To) -->
    seqDelimited(' ', number, [Dest, Start, Length]),
    ( "\n"; call(eos) ),
    { Diff #= Dest - Start,
      UpTo #= Start + Length,
      asserta((rel(From, To, X, Y) :- X #>= Start, X #< UpTo, Y #= X + Diff))
    },
    maprelations(From, To).
maprelations(_, _), "\n" --> "\n".
maprelations(_, _) --> call(eos).

input(Seeds) -->
    "seeds: ", seqDelimited(' ', number, Seeds), "\n", maps.

findlocation(Seed, From, Loc) :-
    rel(From, To, Seed, X),
    (To = "location", Loc = X
      ; findlocation(X, To, Loc)
      ).

part1(F, Sol) :-
    abolish(rel/4),
    phrase_from_file(input(Seeds), F),
    maplist(\X^Y^findlocation(X, "seed", Y), Seeds, Locations),
    list_min(Locations, Sol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Part 2                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% extracts the domain between two variables
mkRange(Min, Max, Dom) :-
  Var #>= Min, Var #< Max, fd_dom(Var, Dom).

maprelations2(From, To, [Range-Fun|More]) -->
    seqDelimited(' ', number, [Dest, Start, Length]),
    ( "\n"; call(eos) ),
    { Diff #= Dest - Start,
      UpTo #= Start + Length,
      mkRange(Start, UpTo, Range),
      Fun = plus(Diff)
    },
    maprelations2(From, To, More).
maprelations2(_, _, []), "\n" --> "\n".
maprelations2(_, _, []) --> call(eos).

maps2([]) --> call(eos).
maps2([Flattened|Next]) --> "\n",
         seqof(alpha, From),
         "-to-",
         seqof(alpha, To),
         " map:\n",
         maprelations2(From, To, Ranges),
         { deconstructFunction(Ranges, Flattened) },
         maps2(Next).

input2(Seeds, Flattened) -->
    "seeds: ", seqDelimited(' ', number, Seeds), "\n", maps2(Flattened).

% Breaks the function by disjoint segments
deconstructFunction(L, Res) :-
    reverse(L, [X|L1]),
    foldl(deconstructFunction, L1, [X], Res).
deconstructFunction(NewRange-NewFun, Disjoints, NewDisjoints) :-
    maplist(fst, Disjoints, DisjointRanges),
    foldl1(or, DisjointRanges, WholeRange),
    ( X in NewRange,
      #\ X in WholeRange,
      fd_dom(X, Dom),
      NewDisjoints = [Dom-NewFun|Disjoints]
    ; NewDisjoints = Disjoints
    ).


% applies a function to the intersection between the range on which the function
% applies and the requested range
applyOnRange(InputRange, Range-Func, RX) :-
    X in Range,
    X in InputRange,
    fd_dom(X, Dom),
    applyOnRange_(Dom, Func, RX).
% applies a function to a range
applyOnRange_(A..B, F, C..D) :- call(F, A, C), call(F, B, D).
applyOnRange_(A\/B, F, C\/D) :- applyOnRange_(A, F, C), applyOnRange_(B, F, D).
applyOnRange_(A, F, C) :- call(F, A, C).

% Transform a range over all the disjoint functions
transformRange([], InputRange, InputRange).
transformRange(InputDisjoints, InputRange, ResultingRange) :-
    InputDisjoints = [Head|DisjointRangesFuncs],
    % apply the first one, we don't have a mempty for ranges
    applyOnRange(InputRange, Head, R1),
    % fold over the rest
    foldl(InputRange+\RR^D1^D2^(
            applyOnRange(InputRange, RR, RX),
            D2 = D1\/RX
          ; % if function doesn't intersect, then this application doesn't grow
            % the domain.
            D2 = D1
          ),
          DisjointRangesFuncs,
          R1,
          ResultingRange1),

    % find the range that applies to no function
    K in InputRange,
    (  maplist(K+\ (Xx-_)^(#\ K in Xx), InputDisjoints),
       fd_dom(K, UnmappedRanges),
       W in ResultingRange1\/UnmappedRanges
    ;
       % if all the data is applied through a function
       W in ResultingRange1
    ),
    fd_dom(W, ResultingRange)

  ; % if the first function doesn't intersect, try with the next one
    InputDisjoints = [Head|DisjointRangesFuncs],
    transformRange(DisjointRangesFuncs, InputRange, ResultingRange).

% Creates the initial range using \/
startingRange([X, Y], X..Z) :-
    Z #= X + Y - 1.
startingRange([X, Y|More], Res) :-
    startingRange(More, Res1),
    Z #= X + Y - 1,
    Res = X..Z\/Res1.

part2(F, Sol) :-
    phrase_from_file(input2(Seeds, Flattened), F),
    startingRange(Seeds, StartRange),
    foldl(transformRange, Flattened, StartRange, EndRange),
    X in EndRange,
    fd_inf(X, Sol).
