:- use_module(library(dcgs)).
:- use_module(csvsep).

eos([], []).

workflow(w(Name, Rules1)) -->
    seq(Name),
    "{",
    parse_csv(frame([],[Rules0]), [with_header(false), line_separator('}')]),
    { maplist(\Rule^(Rule1)^(
                  phrase(parse_csv(frame([], [[[Field, Op|ValueC], Res]]),
                                   [with_header(false), token_separator(':')]),
                         Rule),
                  atom_chars(Op1, [(#), Op]),
                  number_chars(Value, ValueC),
                  Rule1 = r(Field, Op1, Value, Res)
              ; Rule1 = r(Rule)
              ),
              Rules0,
              Rules1)
    },
    "}".

workflows(A, A) --> "\n".
workflows(A, Workflows) -->
    workflow(w(Name, Rules)),
    "\n",
    { put_assoc(Name, A, Rules, A1) },
    workflows(A1, Workflows).

part(p(X,M,A,S)) -->
    "{",
    parse_csv(frame([], [Raw]), [with_header(false), line_separator('}')]),
    { maplist( \ [_,_|Rest]^Num^number_chars(Num, Rest), Raw, [X, M, A, S] ) },
    "}".

parts([]) --> ("\n" ; call(eos)).
parts([P|Parts]) -->
    part(P),
    "\n",
    parts(Parts).

input(A, Workflows, Parts) --> workflows(A, Workflows), parts(Parts).

x(p(X,_,_,_), X).
m(p(_,M,_,_), M).
a(p(_,_,A,_), A).
s(p(_,_,_,S), S).

run_rules([r(Next)], _, Next).
run_rules([r(FieldSelector, Op, N, Res)|Rules], P, Next) :-
    call(FieldSelector, P, Field),
    call(Op, Field, N),
    Next = Res
    ; run_rules(Rules, P, Next).

process_part(CurrWf, Workflows, P, Res) :-
    get_assoc(CurrWf, Workflows, Rules),
    run_rules(Rules, P, Next),
    ( Next = "A" -> Res = accept
    ; Next = "R" -> Res = reject
    ; process_part(Next, Workflows, P, Res)
    ).

process_parts(Acc, _, [], Acc).
process_parts(Acc, Workflows, [P|Parts], Sol) :-
    process_part("in", Workflows, P, Res),
    ( Res = accept,
      P = p(X, M, A, S),
      Acc1 #= Acc + X + M + A + S,
      process_parts(Acc1, Workflows, Parts, Sol)
    ; Res = reject,
      process_parts(Acc, Workflows, Parts, Sol)
      ).

part1(File, Sol) :-
    empty_assoc(A),
    phrase_from_file(input(A, Workflows, Parts), File), !,
    process_parts(0, Workflows, Parts, Sol).

inv(#<, #>=).
inv(#>, #=<).

combine(_, mt, mt, _) :- !, fail.
combine(c(Selector, Op, N), mt, C, CN) :-
    inv(Op, OpInv),
    maplist(append([c(Selector, OpInv, N)]), C, CN).
combine(c(Selector, Op, N), C, mt, CN) :-
    maplist(append([c(Selector, Op, N)]), C, CN).
combine(c(Selector, Op, N), C1, C2, CN) :-
    maplist(append([c(Selector, Op, N)]), C1, C1N),
    inv(Op, OpInv),
    maplist(append([c(Selector, OpInv, N)]), C2, C2N),
    append(C1N, C2N, CN).

rule_tree([r("R")], _, _) :- !, fail.
rule_tree([r("A")], _, [[accept]]).
rule_tree([r(Next)], Workflows, Constraints) :-
    run(Next, Workflows, Constraints).

rule_tree([r(Selector, Op, N, Next)|Rules], Workflows, Constraints) :-
    ( run(Next, Workflows, ConstraintsTrue)
    ; ConstraintsTrue = mt
    ),
    ( rule_tree(Rules, Workflows, ConstraintsFalse)
    ; ConstraintsFalse = mt
    ),
    combine(c(Selector, Op, N), ConstraintsTrue, ConstraintsFalse, Constraints).

run("R", _, _) :- !, fail.
run("A", _, [[accept]]).
run(CurrWf, Workflows, Constraints) :-
    get_assoc(CurrWf, Workflows, Rules),
    rule_tree(Rules, Workflows, Constraints).

path_to_dom_(p(X,M,A,S), [accept]) :-
    [X, M, A, S] ins 1..4000.
path_to_dom_(P, [c(Selector, Op, N)|Path]) :-
    call(Selector, P, Field),
    call(Op, Field, N),
    path_to_dom_(P, Path).
path_to_dom(Path, Domain) :-
    path_to_dom_(p(X,M,A,S), Path),
    maplist(fd_size, [X,M,A,S], [DX, DM, DA, DS]),
    Domain #= DX * DM * DA * DS.

part2(File, Sol) :-
    empty_assoc(A),
    phrase_from_file(input(A, Workflows, _), File),
    run("in", Workflows, PathsToA),
    maplist(path_to_dom, PathsToA, DomainsSize),
    sum_list(DomainsSize, Sol).
