slp_in_options( Opts, OutSlp ) :-
	( memberchk(slp(Slp),Opts) ->
		( (file_exists(Slp); atom_concat(_,'.slp',Slp)) ->
			OutSlp = Slp
			;
			atom_concat( Slp, '.slp', OutSlp )
		)
		;
		memberchk( stem(Stem), Opts ),
		atom_concat( Stem, '.slp', OutSlp )
	).
