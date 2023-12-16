:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(dcgs/debug).
:- use_module(library(dif)).

hash_(N, [], N).
hash_(N, [X|Xs], M) :-
    char_code(X, Code),
    N1 #= N + Code,
    N2 #= N1 * 17,
    N3 #= N2 rem 256,
    hash_(N3, Xs, M).
hash(X, Y) :-
    hash_(0, X, Y).

eos([], []).

word(X), "," --> { maplist(\Y^(dif(',', Y), dif('\n', Y)), X), X = [_|_] }, seq(X), ("," ; "\n" ; call(eos)).
parse([]) --> call(eos).
parse([X|Xs]) -->
    word(X), ",",  parse(Xs).

part1(File, Sol) :-
    phrase_from_file(parse(Xs), File),
    maplist(hash, Xs, Ys),
    sum_list(Ys, Sol).

has_equal(In, In) :- member('=', In).

text_to_label_and_op(a(Hash, Label, Lens)) -->
    has_equal,
    { maplist(dif('='), Label) },
    seq(Label),
    "=",
    seq(LensS),
    call(eos),
    { maplist(\X^char_type(X, numeric), LensS) },
    { number_chars(Lens, LensS), hash(Label, Hash) }.
text_to_label_and_op(r(Hash, Label)) -->
    { maplist(dif('='), Label) },
    seq(Label),
    "-",
    { hash(Label, Hash) }.

% relates a list to the result of replacing or adding a(X,L) at the end.
replace_or_add([], a(X, L), [X-L]).
replace_or_add([X-_| Xs], a(X, L), [X-L | Xs]).
replace_or_add([X-L| Xs], a(Y, L1), [X-L|Ys]) :-
    dif(X, Y),
    replace_or_add(Xs, a(Y, L1), Ys).

remove([], _, []).
remove([X-_|Xs], X, Xs).
remove([Y-Z|Xs], X, [Y-Z|Ys]) :-
    dif(Y, X),
    remove(Xs, X, Ys).

assoc_do(a(Hash, Label, Lens), A, AN) :-
    get_assoc(Hash, A, Box1, AN, Box2),
    replace_or_add(Box1, a(Label, Lens), Box2).

assoc_do(a(Hash, Label, Lens), A, AN) :-
    \+ get_assoc(Hash, A, _),
    put_assoc(Hash, A, [Label-Lens], AN).

assoc_do(r(Hash, Label), A, AN) :-
    get_assoc(Hash, A, Box1),
    Box1 = [Label-_],
    del_assoc(Hash, A, _, AN).

assoc_do(r(Hash, Label), A, AN) :-
    get_assoc(Hash, A, Box1, AN, Box2),
    (Box = [Other-_], dif(Label, Other) ; Box1 = [_,_|_] ),
    remove(Box1, Label, Box2).

assoc_do(r(Hash, _), A, A) :-
    \+ get_assoc(Hash, A, _).

focusing_power_box(Acc, _, _, [], Acc).
focusing_power_box(Acc, BoxIdx, LensIdx, [_-X|Xs], Acc2) :-
    Acc1 #= Acc + ((BoxIdx + 1) * LensIdx * X),
    write([Acc, Acc1]), nl,
    LensIdx1 #= LensIdx + 1,
    focusing_power_box(Acc1, BoxIdx, LensIdx1, Xs, Acc2).

focusing_power(Acc, [], Acc).
focusing_power(Acc, [BoxIdx-L|Boxes], Sol) :-
    focusing_power_box(0, BoxIdx, 1, L, FP),
    Acc1 #= FP + Acc,
    focusing_power(Acc1, Boxes, Sol).


part2(File, Sol) :-
    phrase_from_file(parse(Xs), File),
    maplist(\X^Y^phrase(text_to_label_and_op(Y), X), Xs, Ys), !,
    empty_assoc(A),
    foldl(\Z^B^C^(assoc_do(Z, B, C)), Ys, A, AN),
    assoc_to_list(AN, Sols), !,
    focusing_power(0, Sols, Sol).
