/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(code, []).

transform(proc_body(Name, SemDecls),
          [ push(ix),           % Save frame pointer
            ld(ix, 0),          % Set new frame pointer: ld ix, sp
            add(ix, sp),
            %  sub(sp, size),   % TODO Reserve space for locals 
            %                   % TODO do stuff
            ld(sp, ix),         % Free space for local variables
            pop(ix),            % Restore frame pointer
            ret
          ]).                   % Return

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