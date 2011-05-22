/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(lexer, []).

scan([], []).
% Separators
scan([0'\t|Chars], Tokens) :- !, scan(Chars, Tokens). % CHARACTER TABULATION
scan([0'\n|Chars], Tokens) :- !, scan(Chars, Tokens). % LINE FEED
scan([0'\v|Chars], Tokens) :- !, scan(Chars, Tokens). % LINE TABULATION
scan([0'\f|Chars], Tokens) :- !, scan(Chars, Tokens). % FORM FEED
scan([0'\r|Chars], Tokens) :- !, scan(Chars, Tokens). % CARRIAGE RETURN
scan([0'\ |Chars], Tokens) :- !, scan(Chars, Tokens). % SEPARATOR SPACE
% Compound delimiters
scan([0'=, 0'>|Chars], ['=>'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'., 0'.|Chars], ['..'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'*, 0'*|Chars], ['**'|Tokens]) :- !, scan(Chars, Tokens).
scan([0':, 0'=|Chars], [':='|Tokens]) :- !, scan(Chars, Tokens).
scan([0'/, 0'=|Chars], ['/='|Tokens]) :- !, scan(Chars, Tokens).
scan([0'>, 0'=|Chars], ['>='|Tokens]) :- !, scan(Chars, Tokens).
scan([0'<, 0'=|Chars], ['<='|Tokens]) :- !, scan(Chars, Tokens).
scan([0'<, 0'<|Chars], ['<<'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'>, 0'>|Chars], ['>>'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'<, 0'>|Chars], ['<>'|Tokens]) :- !, scan(Chars, Tokens).
% Delimiters
scan([0'& |Chars], ['&'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'' |Chars], ['\''|Tokens]) :- !, scan(Chars, Tokens).
scan([0'( |Chars], ['('|Tokens]) :- !, scan(Chars, Tokens).
scan([0') |Chars], [')'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'* |Chars], ['*'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'+ |Chars], ['+'|Tokens]) :- !, scan(Chars, Tokens).
scan([0', |Chars], [','|Tokens]) :- !, scan(Chars, Tokens).
scan([0'- |Chars], ['-'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'. |Chars], ['.'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'/ |Chars], ['/'|Tokens]) :- !, scan(Chars, Tokens).
scan([0': |Chars], [':'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'; |Chars], [';'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'< |Chars], ['<'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'= |Chars], ['='|Tokens]) :- !, scan(Chars, Tokens).
scan([0'> |Chars], ['>'|Tokens]) :- !, scan(Chars, Tokens).
scan([0'| |Chars], ['|'|Tokens]) :- !, scan(Chars, Tokens).
% Identifiers and Keywords '
scan([Char|Chars], [Token|Tokens]) :-
        identifier_start_char(Char),
        !,
        span(identifier_extend_char, Chars, Extend, RestChars),
        atom_codes(Atom, [Char|Extend]),
        upcase_atom(Atom, Identifier),
        (  keyword(Identifier)
        -> Token = Identifier
        ;  Token = id(Identifier)
        ),
        scan(RestChars, Tokens).

identifier_start_char(C) :-
        code_type(C, alpha).

identifier_extend_char(C) :-
        code_type(C, alnum).

keyword('BEGIN').
keyword('END').
keyword('IS').
keyword('PROCEDURE').

% span(+Pred, +List, -Prefix, -Remainder) 
span(Pred, [X|Xs], [X|Ps], Rs) :-
        call(Pred, X),
        !,
        span(Pred, Xs, Ps, Rs).
span(_, Xs, [], Xs).


%-----------------------------------------------------------------------

:- begin_tests(lexer).

test(delimiters) :-
        scan("&'()*+,-./:<|=;>",
             ['&', '\'', '(', ')', '*', '+', ',', '-',
              '.', '/', ':', '<', '|', '=', ';', '>']).
test(compount_delimiters) :-
        scan("=>..**:=/=>=<=<<>><>",
             ['=>', '..', '**', ':=', '/=', '>=', '<=', '<<', '>>', '<>']).

test(identifiers) :-
        scan("xyz  X0",
             [id('XYZ'), id('X0')]).

test(keywords) :-
        scan("begin end is procedure",
             ['BEGIN', 'END', 'IS', 'PROCEDURE']).
             

:- end_tests(lexer).