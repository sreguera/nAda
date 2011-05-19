/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(nada, []).

:- use_module(parser).
:- use_module(sem).

parse(Input, Output) :-
        parser:parse(Input, AST),
        sem:transform(AST, Output).

%-----------------------------------------------------------------------

:- begin_tests(nada).

test(procedure_with_declarations) :-
        parse(['procedure', id('X'), 'is',
                   id('a'), ',', id('b'), ':', id('byte'), ';',
                   id('c'), ':', id('word'), ';',
                'begin',
                'end', id('X'), ';'],
              proc_body('X',
                        [decl('a', 'byte'),
                         decl('b', 'byte'),
                         decl('c', 'word')])).

:- end_tests(nada).