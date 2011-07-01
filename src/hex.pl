/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(hex, []).

hex(Data, Address, Hex) :-
        split2(Data, 16, Fragments),
        hex_loop(Fragments, Address, Records0),
        end_record(E),
        append(Records0, [E], Records1),
        append(Records1, Hex).

hex_loop([], _, []).
hex_loop([F|Fs], A0, [R, [0'\n]|Rs]) :- %'
        data_record(A0, F, R),
        A1 is A0 + 16,
        hex_loop(Fs, A1, Rs).

data_record(Address, Data, Record) :-
        hex_record(Address, 0, Data, Record).

end_record(E) :-
        hex_record(0, 1, [], E).

hex_record(Address, Type, Data, [0':|Codes]) :- %'
        word_bytes(Address, AH, AL),
        length(Data, Count),
        checksum([Count, AH, AL, Type|Data], Checksum),
        flatten([Count, AH, AL, Type, Data, Checksum], Bytes),
        hexify(Bytes, Codes).

word_bytes(Word, BH, BL) :-
        BL is Word /\ 0xFF,
        BH is (Word >> 8) /\ 0xFF.
        
checksum(List, Checksum) :-
        sumlist(List, Sum),
        Checksum is (0x100 - (Sum /\ 0xFF)) /\ 0xFF.

hexify([], []).
hexify([B|Bs], [CH, CL|Cs]) :-
        hex_byte(B, CH, CL),
        hexify(Bs, Cs).

hex_byte(B, CH, CL) :-
        BH is (B >> 4) /\ 0xF,
        hex_code(BH, CH),
        BL is B /\ 0xF,
        hex_code(BL, CL).

hex_code(0x0, 0'0).
hex_code(0x1, 0'1).
hex_code(0x2, 0'2).
hex_code(0x3, 0'3).
hex_code(0x4, 0'4).
hex_code(0x5, 0'5).
hex_code(0x6, 0'6).
hex_code(0x7, 0'7).
hex_code(0x8, 0'8).
hex_code(0x9, 0'9).
hex_code(0xA, 0'A).
hex_code(0xB, 0'B).
hex_code(0xC, 0'C).
hex_code(0xD, 0'D).
hex_code(0xE, 0'E).
hex_code(0xF, 0'F).


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