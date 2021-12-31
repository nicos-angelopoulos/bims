increase_contiguous_count_dl( Curr, CurrCnt, Term, NxCurr, NxCnt, List, Tail ) :-
	( \+ Curr = Term ->
		NxCurr = Term,
		NxCnt is 1,
		List = [CurrCnt-Curr|Tail]
		;
		NxCurr = Curr, 
		NxCnt is CurrCnt + 1, 
		List = Tail
	).
