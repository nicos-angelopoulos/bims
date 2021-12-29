ord_only_add_elem( [], Elem, [Elem] ) :- !.
ord_only_add_elem( [H|T], Elem, NwList ) :-
	compare( Ord, H, Elem ),
	( Ord == > ->
		NwList = [Elem,H|T]
		;
		Ord == <,
		NwList = [H|NwT]
	),
	ord_only_add_elem( T, Elem, NwT ).
