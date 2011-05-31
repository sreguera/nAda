/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(asm, []).


assemble([], []).
assemble([I|Is], [C|Cs]) :-
        assemble(I, C),
        assemble(Is, Cs).
assemble(inst(ret), 0xC9).

%-----------------------------------------------------------------------

:- begin_tests(asm).

test(ret) :-
        assemble([inst(ret)],
                 [0xC9]).

:- end_tests(asm).