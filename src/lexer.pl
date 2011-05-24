/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(lexer, []).

%-----------------------------------------------------------------------
% 2.2 Lexical Elements, Separators and Delimiters

scan([], []).
scan([Char|Chars], Tokens) :-
        separator(Char),
        !,
        scan(Chars, Tokens).
scan([Char1, Char2|Chars], [Token|Tokens]) :-
        compound_delimiter(Char1, Char2),
        !,
        atom_codes(Token, [Char1, Char2]),
        scan(Chars, Tokens).        
scan([Char|Chars], [Token|Tokens]) :-
        delimiter(Char),
        !,
        atom_codes(Token, [Char]),
        scan(Chars, Tokens).
scan([Char|Chars], [Token|Tokens]) :-
        identifier_start_char(Char),
        !,
        span(identifier_extend_char, Chars, Extend, RestChars),
        atom_codes(Atom, [Char|Extend]),
        upcase_atom(Atom, Identifier),
        (  reserved_word(Identifier)
        -> Token = Identifier
        ;  Token = id(Identifier)
        ),
        scan(RestChars, Tokens).

separator(0'\t). %'% CHARACTER TABULATION
separator(0'\n). %'% LINE FEED
separator(0'\v). %'% LINE TABULATION
separator(0'\f). %'% FORM FEED
separator(0'\r). %'% CARRIAGE RETURN
separator(0'\ ). %'% SEPARATOR SPACE

delimiter(0'&). %'% ampersand
delimiter(0''). %'% apostrophe, tick
delimiter(0'(). %'% left parenthesis
delimiter(0')). %'% right parenthesis
delimiter(0'*). %'% asterisk, multiply
delimiter(0'+). %'% plus sign
delimiter(0',). %'% comma
delimiter(0'-). %'% hyphen-minus, minus
delimiter(0'.). %'% full stop, dot, point
delimiter(0'/). %'% solidus, divide
delimiter(0':). %'% colon
delimiter(0';). %'% semicolon
delimiter(0'<). %'% less-than sign
delimiter(0'=). %'% equals sign
delimiter(0'>). %'% greater-than sign
delimiter(0'|). %'% vertical line

compound_delimiter(0'=, 0'>). % arrow
compound_delimiter(0'., 0'.). % double dor
compound_delimiter(0'*, 0'*). % double star, exponentiate
compound_delimiter(0':, 0'=). % assignment (pronounced: "becomes") 
compound_delimiter(0'/, 0'=). % inequality (pronounced: "not equals")
compound_delimiter(0'>, 0'=). % greater than or equal
compound_delimiter(0'<, 0'=). % less than or equal
compound_delimiter(0'<, 0'<). % left label bracket
compound_delimiter(0'>, 0'>). % right label bracket
compound_delimiter(0'<, 0'>). % box

%-----------------------------------------------------------------------
% 2.3 Identifiers

identifier_start_char(C) :-
        code_type(C, alpha).

identifier_extend_char(C) :-
        code_type(C, alnum).

%-----------------------------------------------------------------------
% 2.9 Reserved Words

reserved_word('ABORT').
reserved_word('ABS').
reserved_word('ABSTRACT').
reserved_word('ACCEPT').
reserved_word('ACCESS').
reserved_word('ALIASED').
reserved_word('ALL').
reserved_word('AND').
reserved_word('ARRAY').
reserved_word('AT').
reserved_word('BEGIN').
reserved_word('BODY').
reserved_word('CASE').
reserved_word('CONSTANT').
reserved_word('DECLARE').
reserved_word('DELAY').
reserved_word('DELTA').
reserved_word('DIGITS').
reserved_word('DO').
reserved_word('ELSE').
reserved_word('ELSIF').
reserved_word('END').
reserved_word('ENTRY').
reserved_word('EXCEPTION').
reserved_word('EXIT').
reserved_word('FOR').
reserved_word('FUNCTION').
reserved_word('GENERIC').
reserved_word('GOTO').
reserved_word('IF').
reserved_word('IN').
reserved_word('INTERFACE').
reserved_word('IS').
reserved_word('LIMITED').
reserved_word('LOOP').
reserved_word('MOD').
reserved_word('NEW').
reserved_word('NOT').
reserved_word('NULL').
reserved_word('OF').
reserved_word('OR').
reserved_word('OTHERS').
reserved_word('OUT').
reserved_word('OVERRIDING').
reserved_word('PACKAGE').
reserved_word('PRAGMA').
reserved_word('PRIVATE').
reserved_word('PROCEDURE').
reserved_word('PROTECTED').
reserved_word('RAISE').
reserved_word('RANGE').
reserved_word('RECORD').
reserved_word('REM').
reserved_word('RENAMES').
reserved_word('REQUEUE').
reserved_word('RETURN').
reserved_word('REVERSE').
reserved_word('SELECT').
reserved_word('SEPARATE').
reserved_word('SOME').
reserved_word('SUBTYPE').
reserved_word('SYNCHRONIZED').
reserved_word('TAGGED').
reserved_word('TASK').
reserved_word('TERMINATE').
reserved_word('THEN').
reserved_word('TYPE').
reserved_word('UNTIL').
reserved_word('USE').
reserved_word('WHEN').
reserved_word('WHILE').
reserved_word('WITH').
reserved_word('XOR').

%-----------------------------------------------------------------------
% Utils

%% span(+Pred, +List, -Prefix, -Remainder)
% Prefix is the longest prefix of elements of List that satisfies Pred
% Remainder is the remainder of the list
span(Pred, [Elem|List], [Elem|Prefix], Remainder) :-
        call(Pred, Elem),
        !,
        span(Pred, List, Prefix, Remainder).
span(_, Remainder, [], Remainder).

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

test(reserved_words) :-
        scan("begin end is procedure",
             ['BEGIN', 'END', 'IS', 'PROCEDURE']).
             

:- end_tests(lexer).