/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(code, []).

transform(proc_body(Name, SemDecls),
          [inst(push, ix),      % Save frame pointer
           inst(ld, ix, 0),     % Set new frame pointer: ld ix, sp
           inst(add, ix, sp),
          %  inst(sub, sp, size), % TODO Reserve space for locals 
          %                       % TODO do stuff
           inst(ld, sp, ix),    % Free space for local variables
           inst(pop, ix),       % Restore frame pointer
           inst(ret)]).         % Return

%-----------------------------------------------------------------------

:- begin_tests(code).

test(procedure_with_declarations) :-
        transform(proc_body('X',
                            [entity('A', object, 'BYTE'),
                             entity('B', object, 'BYTE'),
                             entity('C', object, 'WORD')]),
                  [inst(push, ix),
                   inst(ld, ix, 0),
                   inst(add, ix, sp),
                   inst(ld, sp, ix), 
                   inst(pop, ix),
                   inst(ret)]).


:- end_tests(code).