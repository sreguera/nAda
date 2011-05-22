/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(nada, []).

:- use_module(lexer).
:- use_module(parser).
:- use_module(sem).

parse(Input, Output) :-
        lexer:scan(Input, Tokens),
        parser:parse(Tokens, AST),
        sem:transform(AST, Output).

%-----------------------------------------------------------------------

:- begin_tests(nada).

test(procedure_with_declarations) :-
        parse("procedure X is
                 a, b : byte;
                 c : word;
               begin
               end X;
              ",
              proc_body('X',
                        [decl('A', 'BYTE'),
                         decl('B', 'BYTE'),
                         decl('C', 'WORD')])).

:- end_tests(nada).