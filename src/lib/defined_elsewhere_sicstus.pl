defined_elsewhere( Head ) :-
	defined_elsewhere( Head, _Mod ).

defined_elsewhere( Head, Mod ) :-
	% slp:predicate_property( Head, Property ),
	predicate_property( Head, Property ),
	( Property == built_in ->
		Mod = user
		;
		Property = imported_from(Mod)
	),
	!.

defined_elsewhere( Head, Mod ) :-
	functor( Head, Name, Arity ),
	current_predicate( Name/Arity ),
	Mod = user.
