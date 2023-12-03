:- use_module('../utils.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Part 2                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% eagerly consume as many dots
dots(N1) --> ".", dots(N), { N1 #= N + 1 }.
dots(0) --> "".

% eagerly consume as many digits
number([X|Y]) --> [X], { char_type(X, numeric) }, number(Y).
number([]) --> "".

adjacent(X-Y, Len, SymbolX-SymbolY) :-
    SymbolX #>= X - 1,
    SymbolX #=< X + Len,
    2 #> abs(Y - SymbolY).

%% The solution
solve1(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    foldl(solve1, Lines, s(0, [], [], 0), s(_, _, _, Sol)).

solve1(L, S, Res) :- solve1(0, L, S, Res).

solve1(X0, L0, s(Y0, Symbols0, Values0, Acc0), Res) :-
    phrase(dots(Dots), L0, LRest0),
    ( % end of line

        LRest0 = "", !,
        Y1 #= Y0 + 1,
        Res = s(Y1, Symbols0, Values0, Acc0) % finish line

    ; % number

      phrase(number(Num), LRest0, LRest1),
      \+ Num = "", !,
      number_chars(Value, Num),

      % number boundaries
      X1 #= X0 + Dots, % starting point for number
      length(Num, NumLength),
      X2 #= X1 + NumLength, % next index

      ( % number touches symbol

        adjacent(X1-Y0, NumLength, SymbolX-SymbolY),
        member(SymbolX-SymbolY, Symbols0),

        Acc1 #= Acc0 + Value,
        solve1(X2, LRest1, s(Y0, Symbols0, Values0, Acc1), Res)

      ; % number does not touch symbol yet

        Values1 = [v(Value, X1-Y0, NumLength)|Values0],
        Acc1 #= Acc0,
        solve1(X2, LRest1, s(Y0, Symbols0, Values1, Acc1), Res)
      )

    ; % symbol

      LRest0 = [_|LRest1], % must be a symbol
      X1 #= X0 + Dots,

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
      solve1(X2, LRest1, s(Y0, Symbols1, Values1, Acc1), Res)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Part 2                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ignores anything but numbers and '*'.
boring(N1) --> [X],
               { \+ (char_type(X, numeric);  X = (*)) },
               boring(N),
               { N1 #= N + 1 }.
boring(0)  --> "".

% The solution
solve2(F, Sol) :-
    phrase_from_file(lines(Lines), F),
    foldl(solve2, Lines, s(0, [], []), s(_, _, Gears)),
    maplist(\A^Res^( A = _-_-Vals, Vals = [V1, V2],
                     Res #= V1 * V2
                   ; Res #= 0),
            Gears,
            GearResults),
    sum_list(GearResults, Sol).

solve2(L, S, Res) :- solve2(0, L, S, Res).
solve2(X0, L0, s(Y0, Values0, Gears0), Res) :-
    phrase(boring(Dots), L0, LRest),
    ( % end of line

      LRest = "", !,
      Y1 #= Y0 + 1,
      Res = s(Y1, Values0, Gears0) % finish line

    ; % number

      phrase(number(Num), LRest, LRest2),
      \+ Num = "", !,
      number_chars(Value, Num),

      % number boundaries
      X1 #= X0 + Dots, % starting point for number
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

      solve2(X2, LRest2, s(Y0, Values2, Gears1), Res)

    ; % gear

      LRest = [*|LRest2],
      X1 #= X0 + Dots,

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

      solve2(X2, LRest2, s(Y0, Values0, Gears1), Res)
    ).
