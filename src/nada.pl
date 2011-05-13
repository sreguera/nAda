/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(nada, []).

parse(Tokens, AST) :-
        phrase(subprogram_body(AST), Tokens).

%-----------------------------------------------------------------------
% Section 2: Lexical Elements
%-----------------------------------------------------------------------
% 2.3 Identifiers

identifier(X) -->
        [X], {atom(X)}. % TODO

%-----------------------------------------------------------------------
% Section 3: Declarations and Types
%-----------------------------------------------------------------------
% 3.1 Declarations

defining_identifier(Id) -->
        identifier(Id).

%-----------------------------------------------------------------------
% Section 6: Subprograms
%-----------------------------------------------------------------------
% 6.1 Subprogram Declarations

subprogram_specification(Proc_Spec) -->
        procedure_specification(Proc_Spec).

procedure_specification(proc_spec(Name)) -->
        ['procedure'], defining_program_unit_name(Name).

defining_program_unit_name(Name) -->
        defining_identifier(Name).

%-----------------------------------------------------------------------
% 6.3 Subprogram Bodies

subprogram_body(proc_body(Name)) -->
        subprogram_specification(proc_spec(Name)), ['is'],
        ['begin'],
        ['end'], identifier(Name), [';'].        

%-----------------------------------------------------------------------

:- begin_tests(nada).

test(empty_procedure) :-
        phrase(subprogram_body(proc_body('X')),
               ['procedure', 'X', 'is',
                'begin',
                'end', 'X', ';']).

end_tests(nada).