:- use_module(utils).

run :-
  format("Solving day 3~n", []),
  format("Part 1: ~n", []),
  time(part1('inputs/3.txt', Sol1)),
  format("   ", []), isok(Sol1, 521515),
  format("Part 2: ~n", []),
  time(part2('inputs/3.txt', Sol2)),
  format("   ", []), isok(Sol2, 69527306).

% Given X-Y coordinates and the Length of the Value,
% constraint the possible locations of symbols.
adjacent(X-Y, Len, SymbolX-SymbolY) :-
    SymbolX #>= X - 1,
    SymbolX #=< X + Len,
    2 #> abs(Y - SymbolY).

part1(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    foldl(part1, Lines, s(0, [], [], 0), s(_, _, _, Sol)).

part1(Input, S, Res) :- part1(0, Input, S, Res).

part1(_, Input0, s(Y0, Symbols0, Values0, Acc0),  s(Y1, Symbols0, Values0, Acc0)) :-
    phrase(seqof((=('.')), _), Input0, ""),
    Y1 #= Y0 + 1.
part1(X0, Input0, s(Y0, Symbols0, Values0, Acc0), Res) :-
    phrase(seqof((=('.')), Dots), Input0, Input1),
    length(Dots, DotsL),

    phrase(number(Value), Input1, Input2),

    % number boundaries
    X1 #= X0 + DotsL, % starting point for number
    number_chars(Value, Num),
    length(Num, NumLength),
    X2 #= X1 + NumLength, % next index

    ( % number touches symbol

      adjacent(X1-Y0, NumLength, SymbolX-SymbolY),
      member(SymbolX-SymbolY, Symbols0),

      Acc1 #= Acc0 + Value,
      Values1 = Values0

    ; % number does not touch symbol yet

      Values1 = [v(Value, X1-Y0, NumLength)|Values0],
      Acc1 #= Acc0
    ),
    part1(X2, Input2, s(Y0, Symbols0, Values1, Acc1), Res).

part1(X0, Input0, s(Y0, Symbols0, Values0, Acc0), Res) :-
    phrase(seqof((=('.')), Dots), Input0, [_|Input2]),
    length(Dots, DotsL),

    X1 #= X0 + DotsL,

    % store new found symbol
    Symbols1 = [X1-Y0|Symbols0],

    findall(v(Val, Vx-Vy),
            (member(v(Val, Vx-Vy, Len), Values0), adjacent(Vx-Vy, Len, X1-Y0)),
            Adjacents),

    % for each, remove them from pending list and accumulate to the result
    foldl((\V^(Acc-Vals)^(Acc2-Vals2)^(V = (v(Val, Vxy)),
                                       Acc2 #= Acc + Val,
                                       select(v(Val, Vxy, _),
                                              Vals,
                                              Vals2))),
          Adjacents,
          Acc0-Values0,
          Acc1-Values1),

    % move index one more (this symbol char)
    X2 #= X1 + 1,
    part1(X2, Input2, s(Y0, Symbols1, Values1, Acc1), Res).

%% Part 2

% The solution
part2(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    foldl(part2, Lines, s(0, [], []), s(_, _, Gears)),
    maplist(\A^Res^( A = _-_-Vals, Vals = [V1, V2],
                     Res #= V1 * V2
                   ; Res #= 0),
            Gears,
            GearResults),
    sum_list(GearResults, Sol).

is_gear(*).

part2(L, S, Res) :- part2(0, L, S, Res).
part2(_, Input0, s(Y0, Values0, Gears0), s(Y1, Values0, Gears0)) :-
    phrase(seqof(\X^(\+ is_gear(X), char_type(X, ascii_punctuation)), _), Input0, ""),
    Y1 #= Y0 + 1.
part2(X0, Input0, s(Y0, Values0, Gears0), Res) :-
    phrase(seqof(\X^(\+ is_gear(X), char_type(X, ascii_punctuation)), Dots), Input0, Input1),
    length(Dots, DotsL),

    phrase(number(Value), Input1, Input2),

    % number boundaries
    X1 #= X0 + DotsL, % starting point for number
    number_chars(Value, Num),
    length(Num, NumLength),
    X2 #= X1 + NumLength, % next index

    % find touching gears
    findall(X-Y-Nums,
            ( adjacent(X1-Y0, NumLength, X-Y),
              select(X-Y-Nums, Gears0, _)
            ),
            TouchingGears),

    % add this number to their adj lists
    maplist(\A^B^(A = (X-Y-Nums), B = (X-Y-Nums1), Nums1 = [Value|Nums]),
            TouchingGears,
            NewTouchingGears),

    % remove gears with 3 touching numbers already
    filter(\A^(A = (X-Y-Nums), length(Nums, Len), Len #< 3),
           NewTouchingGears,
           NewTouchingGears1),

    % find non-touching gears
    findall(X-Y-Nums,
            ( select(X-Y-Nums, Gears0, _),
              \+ member(X-Y-_, NewTouchingGears)
            ),
            NonTouchingGears),

    % concat touching (filtered) and non-touching
    append(NewTouchingGears1, NonTouchingGears, Gears1),

    % record the value
    Values1 = [v(Value, X1-Y0, NumLength)|Values0],

    % Remove values from far previous lines
    filter(Y0+\V^(V = v(_, _-Y, _), 2 #> (Y0 - Y)), Values1, Values2),

    part2(X2, Input2, s(Y0, Values2, Gears1), Res).
part2(X0, Input0, s(Y0, Values0, Gears0), Res) :-
    phrase(seqof(\X^(\+ is_gear(X), char_type(X, ascii_punctuation)), Dots), Input0, [*|Input2]),
    length(Dots, DotsL),

    X1 #= X0 + DotsL,

    % find all values known touching this gear
    findall(Val,
            ( adjacent(Vx-Vy, VL, X1-Y0),
              member(v(Val, Vx-Vy, VL), Values0)
            ),
            TouchingValues),

    length(TouchingValues, LTouching),

    % add this gear if it is not touching 3 values
    ( LTouching #< 3,
      Gears1 = [X1-Y0-TouchingValues|Gears0]
    ; LTouching #>= 3,
      Gears1 = Gears0
    ),

    % move index one more (this gear char)
    X2 #= X1 + 1,

    part2(X2, Input2, s(Y0, Values0, Gears1), Res).
