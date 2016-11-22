%% call_error( +Goal, +Type, +OnFail ).
%
% Call Goal, and repurt failure as of type Type (using werr/2).
% OnFail defines what to do after printing the message: 
% *fail*, _true_, _caution_, _throw_. or _abort_.
% This is likely a temporary implementation for lib(bims). 
% 
call_error( Goal, _Type, _OnFail ) :-
	call( Goal ),
	!.
% Add a silent Type? to print nothing...
call_error( Goal, Type, OnFail ) :-
	werr( failed_on(Goal), Type ),
	call_error_on_fail( OnFail, Goal ).

call_error_on_fail( true, _G ).
call_error_on_fail( caution, _G ) :-
	werr( continuing_with_no_failure, info ).
call_error_on_fail( fail , _G ) :-
	werr( propagating_failure, info ),
	fail.
call_error_on_fail( throw, Goal  ) :-
	throw( failure_on(Goal) ).
call_error_on_fail( abort, _G ) :-
	werr( aborting, info ),
	abort.
