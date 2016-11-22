nst_length( [], Acc, Acc ).
nst_length( [H|T], Acc, Lgth ) :-
	( is_list(H) -> 
		nst_length( H, Acc, NxAcc )
		;
		NxAcc is Acc + 1
	),
	nst_length( T, NxAcc, Lgth ).
