/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(sem, []).

transform(AST, ASG) :-
        process_subprogram_body(AST, ASG).

process_subprogram_body(proc_body(Name, Decls),
                        proc_body(Name, Symbols)) :-
        process_declarations(Decls, Symbols).

%% process_declarations(+Declarations, -Symbols)
process_declarations(Decls, Symbols) :-
        process_declarations(Decls, [], Symbols).

process_declarations([], S, S).
process_declarations([decl(Ns, Type)|Ds], S0, S) :-
        process_declaration(Ns, Type, S0, S1),
        process_declarations(Ds, S1, S).

process_declaration([], _, S, S).
process_declaration([N|Ns], Type, S0, S) :-
        (  memberchk(symbol(N, _), S0)
        -> throw(duplicated_declaration)
        ;  process_declaration(Ns, Type, [symbol(N, Type)|S0], S)
        ).

%-----------------------------------------------------------------------

:- begin_tests(sem).

test(procedure_with_declarations) :-
        pass_subprogram_body(proc_body('X',
                                       [decl(['a', 'b'], 'byte'),
                                        decl(['c'], 'word')]),
                             proc_body('X',
                                       [symbol('c', 'word'),
                                        symbol('b', 'byte'),
                                        symbol('a', 'byte')])).

test(procedure_with_duplicated_declarations,
     [throws(duplicated_declaration)]) :-
        pass_subprogram_body(proc_body('X',
                                       [decl(['a'], 'byte'),
                                        decl(['a'], 'word')]),
                             _).

test(procedure_with_duplicated_declarations2,
     [throws(duplicated_declaration)]) :-
        pass_subprogram_body(proc_body('X',
                                       [decl(['a', 'a'], 'byte')]),
                             _).

:- end_tests(sem).