/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(hex, []).

%% split(+List, +Pos, -Prefix, -Remainder)
split([], _, [], []) :-
        !.
split([X|Xs], Pos, [X|Ps], Rs) :-
        Pos > 0,
        !,
        Pos1 is Pos - 1,
        split(Xs, Pos1, Ps, Rs).
split(Xs, Pos, [], Xs) :-
        Pos =< 0.

%% split2(+List, +Size, -Fragments)
split2([], _, []) :-
        !.
split2(List, Size, [Prefix|Fragments]) :-
        Size > 0,               % TODO throw?
        split(List, Size, Prefix, Remainder),
        split2(Remainder, Size, Fragments).