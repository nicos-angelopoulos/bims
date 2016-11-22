%% debugging_status( +Topic, -Status ).
%
% Status == true iff debugging(Topic) succeeds. Else, it is false.
% Similar to debugging/2, but does not fail for undefined Topic.
%==
% ?- debug( something ).
% true.
% ?- debugging_status( something, Some ).
% Some = true.
% ?- debugging_status( some_else, Else ).
% Else = false.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/23
debugging_status( Topic, Status ) :-
	debugging( Topic ),
	!,
	Status = true.
debugging_status( _Topic, false ).
