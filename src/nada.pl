/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(nada, []).

:- use_module(parser).

parse(Input, Output) :-
        parser:parse(Input, Output).

%-----------------------------------------------------------------------

:- begin_tests(nada).

test(procedure_with_declarations) :-
        parse(['procedure', id('X'), 'is',
                   id('a'), ',', id('b'), ':', id('byte'), ';',
                   id('c'), ':', id('word'), ';',
                'begin',
                'end', id('X'), ';'],
              proc_body('X',
                        [decl(['a', 'b'], 'byte'),
                         decl(['c'], 'word')])).

:- end_tests(nada).