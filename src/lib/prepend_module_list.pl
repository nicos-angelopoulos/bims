:- ensure_loaded( defined_elsewhere ).		%/2.

prepend_module_list( [], _Mod, [] ).
prepend_module_list( [H|T], Mod, [Hp|Tp] ) :-
	% ( predicate_property(H,Property
	( defined_elsewhere( H, There ) ->
		Hp = There:H
		;
		Hp = Mod:H
	),
	prepend_module_list( T, Mod, Tp ).
	
