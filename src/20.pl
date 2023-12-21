:- use_module(library(assoc)).
:- use_module(library(queues)).
:- use_module(library(clpz)).

%%% Utils

repeatN(0, _, In, In).
repeatN(N, G_2, In, Out) :-
    N #> 0,
    N1 #= N - 1,
    call(G_2, In,Aux),
    repeatN(N1, G_2, Aux, Out).

%%% DCGs

word([C|CS]) --> [C], {char_type(C, alpha)}, word(CS).
word([]) --> [].

comma_separated([X]), "\n" --> word(Y), ("\n"; call(eos)), { atom_chars(X, Y) }.
comma_separated([X|Xs]) --> word(Y), ", ", comma_separated(Xs), { atom_chars(X, Y) }.

modu(Name, Targets, f(off)) -->
    "%",
    word(N), { atom_chars(Name, N) },
    " -> ",
    comma_separated(Targets).
modu(Name, Targets, c) -->
    "&",
    word(N), { atom_chars(Name, N) },
    " -> ",
    comma_separated(Targets).
modu(broadcaster, Targets, b) -->
    "broadcaster",
    " -> ",
    comma_separated(Targets).

eos([], []).

%% The `&` modules need to know before-hand all their inputs, this fills an
%% assoc with them for each one by searching in the outputs of other modules.
find_c_inputs(Static, States0, StatesN) :-
    assoc_to_list(States0, StatesL),
    assoc_to_list(Static, StaticL),
    maplist( \ (Name-State)^(Name-NewState)^(
               State = b -> NewState = b
	     ; State = f(X) -> NewState = f(X)
	     ; State = c -> (
		   maplist( \ (N-Targets)^MaybeName^(
				member(Name, Targets) -> MaybeName = [N-low]
			    ; MaybeName = []),
			    StaticL,
			    Names),
		   append(Names, NewNames),
		   list_to_assoc(NewNames, NewStateP),
	           NewState = c(NewStateP)
	       )
	     ),
	     StatesL, StatesM),
    list_to_assoc(StatesM, StatesN).

%% States will contain pairs of Name-State, where State is either `b` for the
%% broadcaster, `c` for `&` modules, which will be filled by find_c_inputs later
%% on, and `f(off)` for `%` modules.
%%
%% Statics contains pairs of Name-Targets. These do not change ever.
parse(States, Statics, States, Statics) --> call(eos).
parse(States0, Statics0, States2, Statics2) -->
    modu(Name, Targets, States),
    { put_assoc(Name, States0, States, States1),
      put_assoc(Name, Statics0, Targets, Statics1)
    },
    "\n",
    parse(States1, Statics1, States2, Statics2).

%% run_module(From-Pulse-To, Targets, State0, State1, Pulses).
%%
%% Receive a pulse and produce the next one, modifying the state.
run_module(_-high-_, _, f(A), f(A), []).
run_module(_-low-Name, Targets, f(off), f(on), Outputs) :-
    maplist(\X^(Name-high-X)^true, Targets, Outputs).
run_module(_-low-Name, Targets, f(on), f(off), Outputs) :-
    maplist(\X^(Name-low-X)^true, Targets, Outputs).
run_module(Input-Pulse-Name, Targets, c(Memory), c(Memory1), Output) :-
    put_assoc(Input, Memory, Pulse, Memory1),
    assoc_to_values(Memory1, Memories),
    (  maplist(=(high), Memories)
    -> maplist(\X^(Name-low-X)^true, Targets, Output)
    ;  maplist(\X^(Name-high-X)^true, Targets, Output)
    ).
run_module(_-X-_, Targets, b, b, Output) :-
    maplist(\Y^(broadcaster-X-Y)^true, Targets, Output).

%% Pushing a button runs until there are no more pending messages
push_button(_, States, PendingMessages, States, Counters, Counters) :-
    queue_length(PendingMessages, 0), !.
push_button(Statics, States, PendingMessages, StatesN, NH-NL, Counters) :-
    queue_head(From-Pulse-To, Messages1, PendingMessages),
    (  Pulse = high
    -> N1H #= NH + 1,
       N1L #= NL
    ;  N1H #= NH,
       N1L #= NL + 1
    ),
    (  get_assoc(To, States, State),
       get_assoc(To, Statics, Targets)
    -> run_module(From-Pulse-To, Targets, State, State1, Outputs),
       put_assoc(To, States, State1, States1),
       queue_last_list(Outputs, Messages1, Messages2),
       push_button(Statics, States1, Messages2, StatesN, N1H-N1L, Counters)
    ;  push_button(Statics, States, Messages1, StatesN, N1H-N1L, Counters)
    ).

next_state(Static, s(States0, Seen, MH-ML), s(States1, Seen1, M1H-M1L)) :-
    (  get_assoc(States0, Seen, States1-NH-NL)
    ->
       Seen1 = Seen,
       M1H #= MH + NH,
       M1L #= ML + NL
    ;
       queue(button-low-broadcaster, Messages),
       push_button(Static, States0, Messages, States1, 0-0, NH-NL),
       put_assoc(States0, Seen, States1-NH-NL, Seen1),
       M1H #= MH + NH,
       M1L #= ML + NL
    ).

part1(File, Sol) :-
    phrase_from_file(parse(t, t, States0, Static), File),
    find_c_inputs(Static, States0, States1),
    repeatN(1000,
	    next_state(Static),
	    s(States1, t, 0-0),
	    s(_, _, NH-NL)),
    Sol #= NH * NL.


%%%%%% Part 2, runs out of memory, there has to be a trick

push_button(_, _, States, PendingMessages, States, Counters, Counters) :-
    queue_length(PendingMessages, 0), !.
push_button(Statics, N, States, PendingMessages, StatesN, RX, Counters) :-
    queue_head(From-Pulse-To, Messages1, PendingMessages),
    (  Pulse = low,
       To = rx
    -> RX1 #= RX + 1
    ;  RX1 #= RX
    ),
    (  get_assoc(To, States, State),
       get_assoc(To, Statics, Targets)
    -> run_module(From-Pulse-To, Targets, State, State1, Outputs),
       put_assoc(To, States, State1, States1),
       queue_last_list(Outputs, Messages1, Messages2),
       push_button(Statics, N, States1, Messages2, StatesN, RX1, Counters)
    ;  push_button(Statics, N, States, Messages1, StatesN, RX1, Counters)
    ).

run_until_done(Static, States0, N, Sol) :-
    queue(button-low-broadcaster, Messages),
    push_button(Static, N, States0, Messages, States1, 0, Counter),
    (  Counter #= 1
    -> Sol #= N
    ;  N1 #= N + 1,
       run_until_done(Static, States1, N1, Sol)
    ).

part2(File, Sol) :-
    phrase_from_file(parse(t, t, States0, Static), File),
    find_c_inputs(Static, States0, States1),
    run_until_done(Static, States1, 1, Sol).
