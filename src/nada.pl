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

compile(Args) :-
        Args = [Input_File],
        compile_file(Input_File, 'a.out').

run :-
        current_prolog_flag(argv, Args),
        append(_SysArgs, ['--'|AppArgs], Args),
        !,
        compile(AppArgs).        

%-----------------------------------------------------------------------

:- begin_tests(nada).

test(procedure_with_declarations) :-
        compile("procedure X is
                   a, b : byte;
                   c : word;
                 begin
                 end X;
                ",
               [0xDD, 0xE5,
                0xDD, 0x21, 0x00, 0x00,
                0xDD, 0x39,
                0xDD, 0xF9,
                0xDD, 0xE1,
                0xC9]).

:- end_tests(nada).