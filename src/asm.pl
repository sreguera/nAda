/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(asm, []).

assemble(Insts, Codes) :-
        asm(Insts, Codes0),
        append(Codes0, Codes).

asm([], []).
asm([I|Is], [C|Cs]) :-
        asm_inst(I, C),
        asm(Is, Cs).

asm_inst(add(ix, sp),     [0xDD, 0x39]).
asm_inst(ret,             [0xC9]).
asm_inst(ld(A1, A2), Bytes) :-
        asm_ld(A1, A2, Bytes).
asm_inst(pop(ix),         [0xDD, 0xE1]).
asm_inst(push(ix),        [0xDD, 0xE5]).

asm_ld(ix, NN, [0xDD, 0x21, N0, N1]) :-
        N0 is NN /\ 0xFF,
        N1 is (NN >> 8) /\ 0xFF.
asm_ld(sp, ix, [0xDD, 0xF9]).


%-----------------------------------------------------------------------

:- begin_tests(asm).

test(ret) :-
        assemble([ret], [0xC9]).

test(ld_ix_NN) :-
        assemble([ld(ix, 0xABCD)], [0xDD, 0x21, 0xCD, 0xAB]).
        
test(add_ix_sp) :-
        assemble([add(ix, sp)], [0xDD, 0x39]).

:- end_tests(asm).
