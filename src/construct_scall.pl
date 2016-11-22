spec_constructs_scall( Prd/Arity, Type, Path, PlArgs, Slp ) :-
	( ad_pt:pred_type( Prd/Arity, Type ) ->
		pred_type_constructs_scall( Type, Prd, Path/[], PlArgs, Slp )
		;
		% fixme: use Swi's error handling
		werr( [['Unrecognised predicate call to: ',Prd/Arity,'. Failing...']] ),
		fail
	).

pred_type_constructs_scall( Type, Prd, PathSt, PlArgs, Slp ) :-
% pred_type_constructs_scall( Type, Prd, First, Second, PlArgs, Slp ) :-
	( Type == s  -> 				% stochastic predicate
		Slp =.. [Prd,PathSt|PlArgs]
		;
		( Type == d ->
			Slp =.. [Prd,PathSt,_Lvars|PlArgs]
			;
			% default, Type = ns
			Slp =.. [Prd,_Id,PathSt|PlArgs]
		)
	).
