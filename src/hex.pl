/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(hex, []).


%% format(+Data, +Offset, -Contents)
%
% Format an hex object file Contents for Data starting at Offset.

format(Data, Offset, Contents) :-
        record_length(Len),
        chunk(Data, Len, Chunks),
        format_loop(Chunks, Offset, Records),
        end_of_file_record(EOF_Record),
        flatten([Records|EOF_Record], Contents).

format_loop([], _, []).
format_loop([Chunk|Chunks], Offset, [Record, 0'\n|Records]) :- %'
        data_record(Offset, Chunk, Record),
        length(Chunk, Len),
        Offset1 is Offset + Len,
        format_loop(Chunks, Offset1, Records).


%% record_length(Length)
%
% The record data length must be less than 255.

record_length(16).


%% data_record(+Offset, +Data, -Record)
%
% Record is a data record for Data starting at Offset.

data_record(Offset, Data, Record) :-
        record(Offset, 0, Data, Record).


%% end_of_file_record(-Record)
%
% Record is the end of file record

end_of_file_record(Record) :-
        record(0, 1, [], Record).


%% record(+Offset, +Type, +Data, -Record)
%
% Record is the hex record with the given Offset, Type and Data.
% Each record is of the form:
%     :LLOOOOTTDD...DDCC
% Where:
% - L is the length of the data field
% - O is the offset
% - T is the record type
% - D is the data
% - C is the checksum

record(Offset, Type, Data, [Mark|Codes]) :-
        record_mark(Mark),
        word_bytes(Offset, OH, OL),
        length(Data, Len),
        checksum([Len, OH, OL, Type|Data], Checksum),
        flatten([Len, OH, OL, Type, Data, Checksum], Bytes),
        list_hexes(Bytes, Codes).


%% record_mark(Code)
%
% Character code that marks the beginning of an hex record.

record_mark(0':). %'


%% checksum(+Byte_List, -Checksum)
%
% Checksum is the least significant byte of the 2's complement of the sum
% of the values in Byte_List.

checksum(List, Checksum) :-
        sumlist(List, Sum),
        Checksum is (0x100 - (Sum /\ 0xFF)) /\ 0xFF.


%% list_hexes(+Byte_List, -Hex_List)
%
% Hex_List is a list of hexadecimal character codes corresponding to the
% bytes in Byte_List.

list_hexes([], []).
list_hexes([B|Bs], [CH, CL|Cs]) :-
        byte_hexes(B, CH, CL),
        list_hexes(Bs, Cs).


%% byte_hexes(+Byte, -High_Code, -Low_Code)
%
% Hexadecimal character codes for the nibbles in Byte.

byte_hexes(B, CH, CL) :-
        byte_nibbles(B, NH, NL),
        hex_code(NH, CH),
        hex_code(NL, CL).


%% hex_code(Digit, Code)
%
% Code is the hexadecimal character code for Digit.

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


%% word_bytes(+Word, -High_Byte, -Low_Byte)

word_bytes(Word, BH, BL) :-
        BH is (Word >> 8) /\ 0xFF,
        BL is Word /\ 0xFF.


%% byte_nibbles(+Byte, -High_Nibble, -Low_Nibble)

byte_nibbles(B, NH, NL) :-
        NH is (B >> 4) /\ 0xF,
        NL is B /\ 0xF.


%% split(+List, +Position, -Prefix, -Remainder)
%
% Split List at Position, giving Prefix and Remainder.

split([], _, [], []) :-
        !.
split([X|Xs], Pos, [X|Ps], Rs) :-
        Pos > 0,
        !,
        Pos1 is Pos - 1,
        split(Xs, Pos1, Ps, Rs).
split(Xs, Pos, [], Xs) :-
        Pos =< 0.


%% chunk(+List, +Size, -Chunks)
%
% Split List in chunks of length Size.

chunk([], _, []) :-
        !.
chunk(List, Size, [Prefix|Chunks]) :-
        Size > 0,               % TODO throw?
        split(List, Size, Prefix, Remainder),
        chunk(Remainder, Size, Chunks).