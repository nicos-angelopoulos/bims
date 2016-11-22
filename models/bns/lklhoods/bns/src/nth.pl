% compatibility predicate for SWI
%
nth( N, List, E, Rem ) :-
	nth1( N, List, E, Rem ).

nth( N, List, E ) :-
	nth1( N, List, E ).
