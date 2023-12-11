:- use_module(utils).

maps([In-(OutL-OutR)|Maps]) -->
    "\n",
    word(In),
    " = (",
    seqDelimited(',', word, [OutL, OutR]),
    ")",
    maps(Maps).
maps([]) --> ("\n" ; call(eos)).

parse_problem(Directions, Maps) -->
    seq(Directions), "\n", maps(Maps).

run_until(Directions, [], From, MapsA, Idx, Res) :-
    run_until(Directions, Directions, From, MapsA, Idx, Res).
run_until(Directions, ['R'|Ds], From, MapsA, Idx, Res) :-
    Idx1 #= Idx + 1,
    get_assoc(From, MapsA, _-R),
    ( R = "ZZZ", Res = R-Idx1
    ; run_until(Directions, Ds, R, MapsA, Idx1, Res)
    ).
run_until(Directions, ['L'|Ds], From, MapsA, Idx, Res) :-
    Idx1 #= Idx + 1,
    get_assoc(From, MapsA, L-_),
    ( L = "ZZZ", Res = L-Idx1
    ; run_until(Directions, Ds, L, MapsA, Idx1, Res)
    ).

part1(F, S) :-
    phrase_from_file(parse_problem(Directions, Maps), F),
    empty_assoc(Assoc),
    foldl(\ (In-X)^Ass0^Ass1^(put_assoc(In, Ass0, X, Ass1)), Maps, Assoc, MapsA),
    run_until(Directions, Directions, "AAA", MapsA, 0, _-S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findZs(Assoc, Directions, Start, End) :-
    length(Directions, DL),
    findZs(Assoc, Directions, DL, Directions, 0, Start, [], End).
findZs(Assoc, Directions, DL, [], Idx, Start, Ends, End) :-
    findZs(Assoc, Directions, DL, Directions, Idx, Start, Ends, End).
findZs(Assoc, Directions, DL, ['L'|Ds], Idx, Start, Ends, Sol) :-
    Idx1 #= Idx + 1,
    get_assoc(Start, Assoc, L-_),
    ( L = ['Z'|_],  ( RelIdx #= Idx1 mod DL,
                      member(L-RelIdx, Ends),
                      Sol = Ends
                    ; Ends1 = [L-RelIdx|Ends],
                      findZs(Assoc, Directions, DL, Ds, Idx1, L, Ends1, Sol)
                    )
    ; findZs(Assoc, Directions, DL, Ds, Idx1, L, Ends, Sol)).
findZs(Assoc, Directions, DL, ['R'|Ds], Idx, Start, Ends, Sol) :-
    Idx1 #= Idx + 1,
    get_assoc(Start, Assoc, _-R),
    ( R = ['Z'|_],  ( RelIdx #= Idx1 mod DL,
                      member(R-(RelIdx-_), Ends),
                      Sol = Ends
                    ; Laps #= Idx1 div DL,
                      Ends1 = [R-(RelIdx-Laps)|Ends],
                      findZs(Assoc, Directions, DL, Ds, Idx1, R, Ends1, Sol)
                    )
    ; findZs(Assoc, Directions, DL, Ds, Idx1, R, Ends, Sol)).

part2(F, S) :-
    phrase_from_file(parse_problem(Directions, Maps), F),
    empty_assoc(Assoc),
    foldl(\ (In-(X1-X2))^Ass0^Ass1^(
              maplist(reverse, [In, X1, X2], [In1, X11, X22]),
              put_assoc(In1, Ass0, X11-X22, Ass1)
          ),
          Maps,
          Assoc,
          MapsA),
    maplist(\ (In-_)^(In1)^(reverse(In, In1)), Maps, MapsIn),
    filter( \ (['A'|_])^true, MapsIn, MapsIn2),
    maplist( (MapsA-Directions) +\ Start^End^(findZs(MapsA, Directions, Start, End)), MapsIn2, Ends ),
    maplist(\ (Y)^X^(write(Y), nl, Y = [_-(_-X)]), Ends, Ends2),
    foldl1(lcm, Ends2, S1), length(Directions, DL), S #= S1 * DL.

dot(F) :-
    phrase_from_file(parse_problem(_, Maps), F),
    format("digraph G {\n", []),
    maplist(\ (X-(Y-Z))^(format("\"~s\" -> \"~s\";~n", [X, Y]), format("\"~s\" -> \"~s\";~n", [X, Z])), Maps),
    format("}", []).
