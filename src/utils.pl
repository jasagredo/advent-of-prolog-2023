/**
Provides some utility functions useful for the solutions.
*/

:- module(utils, [lines//1,
                  line//1,
                  eos/2,
                  seqof//2,
                  seqof_//1,
                  whitespace/1,
                  numeric/1,
                  filter/3,
                  forlist/3,
                  number_string/2
                 ]).

% I have these because otherwise I cannot call lambdas or maplist with filter.
%
% ```
% ?- ['src/2'].
%    true.
% ?- solve2('inputs/2.txt', Sol).
%    error(existence_error(procedure,maplist/3),maplist/3).
% ?- solve2('inputs/2.txt', Sol).
%    error(existence_error(procedure,(\)/3),(\)/3).
% ```
%
% But I don't see anything like this in library(lists), so what am I doing
% wrong? ;_;
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).

:- meta_predicate filter(1, ?, ?).
:- meta_predicate forlist(?, 2, ?).
:- meta_predicate seqof(1, ?).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                            File parsing DCGs                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The Power of Prolog
lines([])           --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

line([])     --> ( "\n" ; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

eos([], []).

seqof(P, S), [X] --> seq(S), {maplist(P, S)}, [X], { \+ call(P, X) }.
seqof_(P) --> seqof(P,_).

whitespace(' ').
numeric(X) :- char_type(X, numeric).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Lists                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter(_, [], []).
filter(G_1, [X|Xs], ZZs) :-
    filter(G_1, Xs, Zs),
    ( call(G_1, X)
    -> ZZs = [X|Zs]
    ; ZZs = Zs
    ).

forlist(In, G_2, Out) :- maplist(G_2, In, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Catches                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

number_string(N, S) :-
    catch(number_chars(N, S), _, false).
