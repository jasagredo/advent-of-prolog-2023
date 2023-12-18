:- use_module(library(dcgs)).

%% The Power of Prolog
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

eos([], []).

eager_seq_of(G_1, [E|Es]) --> [E], { call(G_1, E) }, eager_seq_of(G_1, Es).
eager_seq_of(_, []) --> [].
