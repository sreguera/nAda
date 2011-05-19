/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(parser, []).

parse(Tokens, AST) :-
        phrase(subprogram_body(AST), Tokens).

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

object_declaration(decl(Ids, Subtype)) -->
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
        ['procedure'], defining_program_unit_name(Name).

defining_program_unit_name(Name) -->
        defining_identifier(Name).

%-----------------------------------------------------------------------
% 6.3 Subprogram Bodies

subprogram_body(proc_body(Name, Decls)) -->
        subprogram_specification(proc_spec(Name)),
        ['is'],
        declarative_part(Decls),
        ['begin'],
        ['end'], identifier(Name), [';'].        

%-----------------------------------------------------------------------

:- begin_tests(parser).

test(empty_procedure) :-
        phrase(subprogram_body(proc_body('X', [])),
               ['procedure', id('X'), 'is',
                'begin',
                'end', id('X'), ';']).

test(procedure_with_declarations) :-
        phrase(subprogram_body(proc_body('X',
                                         [decl(['a', 'b'], 'byte'),
                                          decl(['c'], 'word')])),
               ['procedure', id('X'), 'is',
                   id('a'), ',', id('b'), ':', id('byte'), ';',
                   id('c'), ':', id('word'), ';',
                'begin',
                'end', id('X'), ';']).

:- end_tests(parser).