:- ensure_loaded( library(lists) ). 	% select/3.

% select_list( These, FromThem, Leaving ) :-
select_once_list( [], Any, Any ) :-
        !.
% select_list( _Any, [], [] ) :-
        % !.
        % just covered from the safe select below
        % if it is faster, depends on the use.

select_once_list( [H|T], From, Answer ) :-
	( select( H, From, To ) -> 
			true
			;
			To = From
	),
	select_once_list( T, To, Answer ).
