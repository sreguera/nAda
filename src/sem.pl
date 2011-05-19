/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(sem, []).

transform(AST, ASG) :-
        pass_subprogram_body(AST, ASG).

pass_subprogram_body(proc_body(Name, Decls),
                     proc_body(Name, SemDecls)) :-
        pass_declarations(Decls, SemDecls).

% pass_declarations(+Declarations, -SemanticDeclarations)
pass_declarations([decl(Ns, Type)|Ds], SDs) :-
        pass_declaration(Ns, Type, HSDs),
        pass_declarations(Ds, TSDs),
        append(HSDs, TSDs, SDs).
pass_declarations([], []).

pass_declaration([N|Ns], Type, [decl(N, Type)|SDs]) :-
        pass_declaration(Ns, Type, SDs).
pass_declaration([], _, []).

        

%-----------------------------------------------------------------------

:- begin_tests(sem).

test(procedure_with_declarations) :-
        pass_subprogram_body(proc_body('X',
                                       [decl(['a', 'b'], 'byte'),
                                        decl(['c'], 'word')]),
                             proc_body('X',
                                       [decl('a', 'byte'),
                                        decl('b', 'byte'),
                                        decl('c', 'word')])).

:- end_tests(sem).