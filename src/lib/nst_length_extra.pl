:- ensure_loaded( library(lists) ).		% is_list/1.
:- ensure_loaded( nst_length ).

% nst_length_extra( +NextList, +LengthSoFar, +Nth, -NPart, -Lgth ) :-
nst_length_extra( [], Acc, _, _, Acc ).
nst_length_extra( [H|T], Acc, Nth, NPart, Lgth ) :-
	( Nth =:= Acc + 1 -> 
		( is_list(H) ->
			nst_length( H, Acc, NPart ),
			nst_length( T, NPart, Lgth )
			;
			nst_length( [H|T], Acc, NPart ),
			Lgth = NPart
		)
		;
		( is_list(H) -> 
			nst_length_extra( H, Acc, Nth, NPart, NxAcc ),
			( var(NPart) ->
				nst_length_extra( T, NxAcc, Nth, NPart, Lgth )
				;
				nst_length( T, NxAcc, Lgth )
			)
			;
			NxAcc is Acc + 1,
			nst_length_extra( T, NxAcc, Nth, NPart, Lgth )
		)
	).
