tuple_to_list( (A,B), [A|BList] ) :-
	!,
	tuple_to_list( B, BList ).
tuple_to_list( Catch, List ) :-
	( Catch = true -> 
		List = []
		;
		List = [Catch]
	).
