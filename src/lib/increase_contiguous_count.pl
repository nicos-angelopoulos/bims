
increase_contiguous_count( [], ReadTerm, [ReadTerm-1] ).
increase_contiguous_count( [H-Hcount|T], ReadTerm, NxAcc ) :-
	( \+ H = ReadTerm ->
		NxAcc = [ReadTerm-1,H-Hcount|T]
		;
		NxHcount is Hcount + 1,
		NxAcc = [H-NxHcount|T]
	).
