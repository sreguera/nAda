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

basic_declaration(Ctx0, Decl, Ctx) -->
        object_declaration(Ctx0, Decl, Ctx).

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

object_declaration(Ctx0, decl(Ids, Subtype), Ctx0) -->
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

declarative_part(Ctx0, [Decl|Decls], Ctx) -->
        declarative_item(Ctx0, Decl, Ctx1), !, % TODO ! to avoid choice point, ok?
        declarative_part(Ctx1, Decls, Ctx).
declarative_part(Ctx, [], Ctx) -->
        [].

declarative_item(Ctx0, Decl, Ctx) -->
        basic_declarative_item(Ctx0, Decl, Ctx).

basic_declarative_item(Ctx0, Decl, Ctx) -->
        basic_declaration(Ctx0, Decl, Ctx).

%-----------------------------------------------------------------------
% Section 6: Subprograms
%-----------------------------------------------------------------------
% 6.1 Subprogram Declarations

subprogram_specification(Ctx0, Proc_Spec, Ctx) -->
        procedure_specification(Ctx0, Proc_Spec, Ctx).

procedure_specification(Ctx0, proc_spec(Name), Ctx) -->
        ['procedure'], defining_program_unit_name(Name),
        { put_symbol(Name, Ctx0, 'proc', Ctx) }.

defining_program_unit_name(Name) -->
        defining_identifier(Name).

%-----------------------------------------------------------------------
% 6.3 Subprogram Bodies

subprogram_body(proc_body(Name, Decls)) -->
        { empty_context(Ctx0) },
        subprogram_specification(Ctx0, proc_spec(Name), Ctx1),
        ['is'],
        declarative_part(Ctx1, Decls, Ctx2),
        { print(Ctx2) },
        ['begin'],
        ['end'], identifier(Name), [';'].        

%-----------------------------------------------------------------------

%% empty_context(Ctx).
empty_context([]).

%% put_symbol(+Name, +Ctx, +Type, -New_Ctx)
put_symbol(Name, Ctx, Type, [Name-Type|Ctx]).

%% get_symbol(+Name, +Ctx, -Type)
get_symbol(Name, Ctx, Type) :-
        memberchk(Name-Type, Ctx). 


%-----------------------------------------------------------------------

:- begin_tests(parser).

test(context) :-
        empty_context(Ctx0),
        put_symbol(a, Ctx0, 1, Ctx1),
        get_symbol(a, Ctx1, 1),
        \+ get_symbol(a, Ctx0, _).

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