defined_elsewhere( Head ) :-
	defined_elsewhere( Head, _Mod ).

defined_elsewhere( Head, Mod ) :-
	% slp:predicate_property( Head, Property ),
	current_module( Amod ),
	predicate_property( Amod:Head, Property ),
	% write( user_error, prop(Property) ), nl( user_error ),
	( Property == built_in ->
		Mod = user
		;
		Property = imported_from(Mod),
		predicate_property( Amod:Head, number_of_clauses(N) ),
		N > 0
	), 
	!.
	% write( user_error, from(Mod)-hd(Head)-n(N) ), nl( user_error ).

defined_elsewhere( Head, Mod ) :-
	functor( Head, Name, Arity ),
	current_predicate( Name/Arity ),
	Mod = user.
