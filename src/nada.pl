/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(nada, []).

:- use_module(lexer).
:- use_module(parser).
:- use_module(sem).
:- use_module(code).
:- use_module(asm).
:- use_module(util).

compile(Input, Output) :-
        lexer:scan(Input, Tokens),
        parser:parse(Tokens, AST),
        sem:transform(AST, ASG),
        code:transform(ASG, Asm),
        asm:assemble(Asm, Output).

compile_file(Input_File, Output_File) :-
        read_file_codes(Input_File, Input),
        compile(Input, Output),
        write_file_bytes(Output_File, Output).

%-----------------------------------------------------------------------

:- begin_tests(nada).

test(procedure_with_declarations) :-
        parse("procedure X is
                 a, b : byte;
                 c : word;
               begin
               end X;
              ",
             [0xC9]).

:- end_tests(nada).