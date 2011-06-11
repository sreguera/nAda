/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(util,
          [ read_codes/2,       % +Stream, -Codes
            read_file_codes/2,  % +File, -Codes
            write_bytes/2,      % +Stream, +Bytes
            write_file_bytes/2  % +File, +Bytes
          ]).

%% read_codes(+Stream, -Codes)
read_codes(Stream, Codes) :-
        get_code(Stream, Code),
        read_codes(Code, Stream, Codes).

read_codes(-1, _, []) :-
        !.
read_codes(C0, Stream, [C0|Codes]) :-
        get_code(Stream, C1),
        read_codes(C1, Stream, Codes).


%% read_file_codes(+File, -Codes)
read_file_codes(File, Codes) :-
        open(File, read, Stream),
        read_codes(Stream, Codes),
        close(Stream).


%% write_bytes(+Stream, +Bytes)
write_bytes(Stream, Bytes) :-
        write_bytes_aux(Bytes, Stream).

write_bytes_aux([], _).
write_bytes_aux([B|Bs], Stream) :-
        put_byte(Stream, B),
        write_bytes_aux(Bs, Stream).


%% write_file_bytes(+File, +Bytes)
write_file_bytes(File, Bytes) :-
        open(File, write, Stream, [type(binary)]),
        write_bytes(Stream, Bytes),
        close(Stream).

