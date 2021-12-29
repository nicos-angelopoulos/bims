rev_append( [], Tail, Tail ).
rev_append( [H|T], App, All ) :-
	rev_append( T, [H|App], All ).
