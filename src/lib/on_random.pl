on_random( Threshold, First, Second, Rnd, Whc, Choose ) :-
	random( Rnd ),
	( Rnd < Threshold ->
		Choose = First, Whc = first
		;
		Choose = Second, Whc = second
	).
