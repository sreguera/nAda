/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(parser, []).

parse(Tokens, AST) :-
        phrase(compilation(AST), Tokens).

%-----------------------------------------------------------------------
% Section 2: Lexical Elements
%-----------------------------------------------------------------------
% 2.3 Identifiers

identifier(X) -->
        [id(X)]. % TODO

%-----------------------------------------------------------------------
% Section 3: Declarations and Types
%-----------------------------------------------------------------------
% 3.1 Declarations

basic_declaration(Decl) -->
        object_declaration(Decl).

defining_identifier(Id) -->
        identifier(Id).


%-----------------------------------------------------------------------
% 3.3 Objects and Named Numbers

% 3.2.2 Subtype Declarations

subtype_indication(Subtype) -->
        subtype_mark(Subtype).

subtype_mark(Subtype) -->
        subtype_name(Subtype).

subtype_name(Subtype) -->
        identifier(Subtype). % TODO

% 3.3.1 Object Declarations

object_declaration(decl(Ids, object, Subtype)) -->
        defining_identifier_list(Ids), [':'],
        subtype_indication(Subtype), [';'].

defining_identifier_list([Id|Ids]) -->
        defining_identifier(Id),
        defining_identifier_list_rest(Ids).

defining_identifier_list_rest([Id|Ids]) -->
        [','], !, defining_identifier(Id), % TODO ! to avoid choice point, ok?
        defining_identifier_list_rest(Ids).
defining_identifier_list_rest([]) -->
        [].

%-----------------------------------------------------------------------
% 3.11 Declarative Parts

declarative_part([Decl|Decls]) -->
        declarative_item(Decl), !, % TODO ! to avoid choice point, ok?
        declarative_part(Decls).
declarative_part([]) -->
        [].

declarative_item(Decl) -->
        basic_declarative_item(Decl).

basic_declarative_item(Decl) -->
        basic_declaration(Decl).

%-----------------------------------------------------------------------
% Section 6: Subprograms
%-----------------------------------------------------------------------
% 6.1 Subprogram Declarations

subprogram_specification(Proc_Spec) -->
        procedure_specification(Proc_Spec).

procedure_specification(proc_spec(Name)) -->
        ['PROCEDURE'], defining_program_unit_name(Name).

defining_program_unit_name(Name) -->
        defining_identifier(Name).

%-----------------------------------------------------------------------
% 6.3 Subprogram Bodies

subprogram_body(proc_body(Name, Decls)) -->
        subprogram_specification(proc_spec(Name)),
        ['IS'],
        declarative_part(Decls),
        ['BEGIN'],
        ['END'], identifier(Name), [';'].        

%-----------------------------------------------------------------------
% Section 10: Program Structure and Compilation Issues
%-----------------------------------------------------------------------
% 10.1 Separate Compilation

% 10.1.1 Compilation Units - Library Units

compilation(C) -->
        compilation_unit(C).

compilation_unit(U) -->
        library_item(U).

library_item(I) -->
        library_unit_body(I).

library_unit_body(B) -->
        subprogram_body(B).

%-----------------------------------------------------------------------

:- begin_tests(parser).

test(empty_procedure) :-
        phrase(subprogram_body(proc_body('X', [])),
               ['PROCEDURE', id('X'), 'IS',
                'BEGIN',
                'END', id('X'), ';']).

test(procedure_with_declarations) :-
        phrase(subprogram_body(proc_body('X',
                                         [decl(['A', 'B'], object, 'BYTE'),
                                          decl(['C'], object, 'WORD')])),
               ['PROCEDURE', id('X'), 'IS',
                   id('A'), ',', id('B'), ':', id('BYTE'), ';',
                   id('C'), ':', id('WORD'), ';',
                'BEGIN',
                'END', id('X'), ';']).

:- end_tests(parser).