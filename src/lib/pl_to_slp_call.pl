pl_to_slp_call( Pl, Slp ) :-
	pl_to_slp_call_args( Pl, Slp, _ ).

pl_to_slp_call_args( Pl, Slp, AllArgs ) :-
	( Pl = (Pfx:PlTerm) ->
		( is_list(Pfx) -> 
			PlTerm =.. [Name|PlArgs],
			length( PlArgs, Arity ),
			( is_distributional( Name/Arity ) ->
				Slp =.. [Name,A,Pfx|PlArgs],
				AllArgs = [type(d),pl_args(PlArgs),path(A),
							label_args(Pfx)]
							% second_path(B),label_args(Pfx)]
				;
				write( user_error, 'pl_to_slp_call_args/3, prefix used with predicate which is not distributional. ' ),
				nl( user_error ),
				write( user_error, Pl ), nl( user_error ), abort
			)
			;
			write( user_error, 'pl_to_slp_call_args/3, non list in prefix. ' ),
			nl( user_error ), abort
		)
		;
		Pl =.. [Name|PlArgs],
		length( PlArgs, Arity ),
		( spec_of_type( Name/Arity, s ) ->
			Slp =.. [Name,A|PlArgs],
			AllArgs = [type(s),pl_args(PlArgs),path(A)]
			% AllArgs = [type(s),pl_args(PlArgs),first_path(A),second_path(B)]
			;
			( spec_of_type( Name/Arity, d ) ->
				Slp =.. [Name,A,C|PlArgs],
				AllArgs = [type(d),pl_args(PlArgs),path(A),label_args(C)]
							% second_path(B),label_args(C)]
				;
				Slp =.. [Name,Id,A|PlArgs],
				AllArgs = [type(ns),pl_args(PlArgs),path(A), id(Id)]
			)
		)
	).
