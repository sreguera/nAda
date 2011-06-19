/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(sem, []).

transform(AST, ASG) :-
        process_compilation(AST, ASG).

process_compilation(AST, ASG) :-
        Env = [entity('BYTE', type, nul),
               entity('WORD', type, nul)],
        process_subprogram_body(AST, Env, ASG).

process_subprogram_body(proc_body(Name, Decls),
                        Env,
                        proc_body(Name, Entities)) :-
        process_declarations(Decls, Env, Entities).

%% process_declarations(+Declarations, -Entities)
process_declarations(Decls, Env, Entities) :-
        process_declarations(Decls, Env, [], Entities).

% process_declarations(+Declarations, +Accumulator, -Result) 
process_declarations([], _, S, S).
process_declarations([D|Ds], Env, S0, S) :-
        D = decl(Names, Type, Data),
        process_declaration(Type, Names, Data, Env, S0, S1),
        process_declarations(Ds, Env, S1, S).

% process_declaration(+Type, +Names, +Data, +Accumulator, +Result)
process_declaration(object, Names, Data, Env, S0, S) :-
        (  memberchk(entity(Data, type, _), Env)
        -> process_object_declaration(Names, Data, S0, S)
        ;  throw(unknown_subtype)
        ).
        
process_object_declaration([], _, S, S).
process_object_declaration([Name|Ns], Subtype, S0, S) :-
        (  memberchk(entity(Name, _, _), S0)
        -> throw(duplicated_declaration)
        ;  E = entity(Name, object, Subtype),
           process_object_declaration(Ns, Subtype, [E|S0], S)
        ).

%-----------------------------------------------------------------------

:- begin_tests(sem).

test(procedure_with_declarations) :-
        process_compilation(proc_body('X',
                                      [decl(['A', 'B'], object, 'BYTE'),
                                       decl(['C'], object, 'WORD')]),
                            proc_body('X',
                                      [entity('C', object, 'WORD'),
                                       entity('B', object, 'BYTE'),
                                       entity('A', object, 'BYTE')])).

test(procedure_with_duplicated_declarations,
     [throws(duplicated_declaration)]) :-
        process_compilation(proc_body('X',
                                      [decl(['A'], object, 'BYTE'),
                                       decl(['A'], object, 'WORD')]),
                            _).

test(procedure_with_duplicated_declarations2,
     [throws(duplicated_declaration)]) :-
        process_compilation(proc_body('X',
                                      [decl(['A', 'A'], object, 'BYTE')]),
                            _).

test(procedure_with_unknow_subtype,
     [throws(unknown_subtype)]) :-
        process_compilation(proc_body('X',
                                      [decl(['A'], object, 'XYZZY')]),
                            _).

:- end_tests(sem).