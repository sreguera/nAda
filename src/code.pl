/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(code, []).


transform(AST, Asm) :-
        transform(AST, Asm, []).

transform(proc_body(_Name, _SemDecls)) -->
        prolog,
        epilog.

prolog -->
        [ push(ix),             % Save frame pointer
          ld(ix, 0),            % Set new frame pointer: ld ix, sp
          add(ix, sp)
        ].

epilog -->
        [ ld(sp, ix),           % Free space for local variables
          pop(ix),              % Restore frame pointer
          ret                   % Return
        ].

%-----------------------------------------------------------------------

:- begin_tests(code).

test(procedure_with_declarations) :-
        transform(proc_body('X',
                            [entity('A', object, 'BYTE'),
                             entity('B', object, 'BYTE'),
                             entity('C', object, 'WORD')]),
                  [push(ix),
                   ld(ix, 0),
                   add(ix, sp),
                   ld(sp, ix), 
                   pop(ix),
                   ret]).

:- end_tests(code).