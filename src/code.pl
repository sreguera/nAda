/*
  Part of nAda.
  Copyright (c) 2011 Jose Sebastian Reguera Candal.
*/
:- module(code, []).

transform(proc_body(Name, SemDecls),
          [inst(ret)]).
          % [inst(push, ix),      % Save frame pointer
          %  inst(ld, ix, sp),    % TODO Set new frame pointer ld ix,0;add ix,sp
          %  inst(sub, sp, size), % TODO Reserve space for locals 
          %                       % TODO do stuff
          %  inst(ld, sp, ix),    % Free space for local variables
          %  inst(pop, ix),       % Restore frame pointer
          %                       % TODO Free space for arguments
          %  inst(ret)]).         % Return

%-----------------------------------------------------------------------

:- begin_tests(code).

test(procedure_with_declarations) :-
        transform(proc_body('X',
                            [decl('a', 'byte'),
                             decl('b', 'byte'),
                             decl('c', 'word')]),
                  [inst(ret)]).


:- end_tests(code).