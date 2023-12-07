/*  This module is a derived form of the standard assoc library of
    Scryer-Prolog and as such, retains the following copyright notice:

    Author:        R.A.O'Keefe, L.Damas, V.S.Costa, Glenn Burgess,
                   Jiri Spitz and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2018, various people and institutions
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(assoc_by,
          [ get_assoc_by/4,                % +Cmp_2, +Key, +Assoc, ?Value
            get_assoc_by/6,                % +Cmp_2, +Key, +Assoc0, ?Val0, ?Assoc, ?Val
            put_assoc_by/5                 % +Cmp_2, +Key, +Assoc0, +Value, ?Assoc
          ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).


/** Binary associations

Assocs are Key-Value associations implemented as  a balanced binary tree
(AVL tree).

Authors: R.A.O'Keefe, L.Damas, V.S.Costa and Jan Wielemaker
*/

:- meta_predicate get_assoc_by(2, ?, ?, ?).
:- meta_predicate get_assoc_by(2, ?, ?, ?, ?, ?).
:- meta_predicate put_assoc_by(2, ?, ?, ?, ?).

%% get_assoc(+Key, +Assoc, -Value) is semidet.
%
% True if Key-Value is an association in Assoc.
%
% Throws error: `type_error(assoc, Assoc)` if Assoc is not an association list.

get_assoc_by(Cmp_2, Key, Assoc, Val) :-
    must_be(assoc, Assoc),
    get_assoc_by_(Cmp_2, Key, Assoc, Val).

/*
:- if(current_predicate('$btree_find_node'/5)).
get_assoc_(Key, Tree, Val) :-
    Tree \== t,
    '$btree_find_node'(Key, Tree, 0x010405, Node, =),
    arg(2, Node, Val).
:- else.
*/
get_assoc_by_(Cmp_2, Key, t(K,V,_,L,R), Val) :-
    call(Cmp_2, Rel, Key, K),
    get_assoc_by(Rel, Key, V, L, R, Val).

get_assoc_by(_, =, _, Val, _, _, Val).
get_assoc_by(Cmp_2, <, Key, _, Tree, _, Val) :-
    get_assoc_by(Cmp_2, Key, Tree, Val).
get_assoc_by(Cmp_2, >, Key, _, _, Tree, Val) :-
    get_assoc_by(Cmp_2, Key, Tree, Val).
% :- endif.


%% get_assoc(+Key, +Assoc0, ?Val0, ?Assoc, ?Val) is semidet.
%
% True if Key-Val0 is in Assoc0 and Key-Val is in Assoc.

get_assoc_by(Cmp_2, Key, t(K,V,B,L,R), Val, t(K,NV,B,NL,NR), NVal) :-
    call(Cmp_2, Rel, Key, K),
    get_assoc_by(Cmp_2, Rel, Key, V, L, R, Val, NV, NL, NR, NVal).

get_assoc_by(_, =, _, Val, L, R, Val, NVal, L, R, NVal).
get_assoc_by(Cmp_2, <, Key, V, L, R, Val, V, NL, R, NVal) :-
    get_assoc_by(Cmp_2, Key, L, Val, NL, NVal).
get_assoc_by(Cmp_2, >, Key, V, L, R, Val, V, L, NR, NVal) :-
    get_assoc_by(Cmp_2, Key, R, Val, NR, NVal).

%% put_assoc(+Key, +Assoc0, +Value, -Assoc) is det.
%
% Assoc is Assoc0, except that Key is associated with
% Value. This can be used to insert and change associations.

put_assoc_by(Cmp_2, Key, A0, Value, A) :-
    insert_by(Cmp_2, A0, Key, Value, A, _).

insert_by(_, t, Key, Val, t(Key,Val,-,t,t), yes).
insert_by(Cmp_2, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
    call(Cmp_2, Rel, K, Key),
    insert_by(Cmp_2, Rel, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged).

insert_by(_, =, t(Key,_,B,L,R), _, V, t(Key,V,B,L,R), no).
insert_by(Cmp_2, <, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
    insert_by(Cmp_2, L, K, V, NewL, LeftHasChanged),
    adjust(LeftHasChanged, t(Key,Val,B,NewL,R), left, NewTree, WhatHasChanged).
insert_by(Cmp_2, >, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
    insert_by(Cmp_2, R, K, V, NewR, RightHasChanged),
    adjust(RightHasChanged, t(Key,Val,B,L,NewR), right, NewTree, WhatHasChanged).

adjust(no, Oldree, _, Oldree, no).
adjust(yes, t(Key,Val,B0,L,R), LoR, NewTree, WhatHasChanged) :-
    table(B0, LoR, B1, WhatHasChanged, ToBeRebalanced),
    rebalance(ToBeRebalanced, t(Key,Val,B0,L,R), B1, NewTree, _, _).

%     balance  where     balance  whole tree  to be
%     before   inserted  after    increased   rebalanced
table(-      , left    , <      , yes       , no    ) :- !.
table(-      , right   , >      , yes       , no    ) :- !.
table(<      , left    , -      , no        , yes   ) :- !.
table(<      , right   , -      , no        , no    ) :- !.
table(>      , left    , -      , no        , no    ) :- !.
table(>      , right   , -      , no        , yes   ) :- !.

% Single and double tree rotations - these are common for insert and delete.
/* The patterns (>)-(>), (>)-( <), ( <)-( <) and ( <)-(>) on the LHS
   always change the tree height and these are the only patterns which can
   happen after an insertion. That's the reason why we can use a table only to
   decide the needed changes.

   The patterns (>)-( -) and ( <)-( -) do not change the tree height. After a
   deletion any pattern can occur and so we return yes or no as a flag of a
   height change.  */


rebalance(no, t(K,V,_,L,R), B, t(K,V,B,L,R), Changed, Changed).
rebalance(yes, OldTree, _, NewTree, _, RealChange) :-
    avl_geq(OldTree, NewTree, RealChange).

avl_geq(t(A,VA,>,Alpha,t(B,VB,>,Beta,Gamma)),
        t(B,VB,-,t(A,VA,-,Alpha,Beta),Gamma), yes) :- !.
avl_geq(t(A,VA,>,Alpha,t(B,VB,-,Beta,Gamma)),
        t(B,VB,<,t(A,VA,>,Alpha,Beta),Gamma), no) :- !.
avl_geq(t(B,VB,<,t(A,VA,<,Alpha,Beta),Gamma),
        t(A,VA,-,Alpha,t(B,VB,-,Beta,Gamma)), yes) :- !.
avl_geq(t(B,VB,<,t(A,VA,-,Alpha,Beta),Gamma),
        t(A,VA,>,Alpha,t(B,VB,<,Beta,Gamma)), no) :- !.
avl_geq(t(A,VA,>,Alpha,t(B,VB,<,t(X,VX,B1,Beta,Gamma),Delta)),
        t(X,VX,-,t(A,VA,B2,Alpha,Beta),t(B,VB,B3,Gamma,Delta)), yes) :-
    !,
    table2(B1, B2, B3).
avl_geq(t(B,VB,<,t(A,VA,>,Alpha,t(X,VX,B1,Beta,Gamma)),Delta),
        t(X,VX,-,t(A,VA,B2,Alpha,Beta),t(B,VB,B3,Gamma,Delta)), yes) :-
    !,
    table2(B1, B2, B3).

table2(< ,- ,> ).
table2(> ,< ,- ).
table2(- ,- ,- ).
