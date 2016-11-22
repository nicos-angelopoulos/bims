:- ensure_loaded( delete_trailing ).		% /3.

number_abbreviation( Repeats, Abrv ) :-
	M is Repeats / 1000000,
	( M >= 1 ->
		number_codes( M, Mcs ),
		delete_trailing( Mcs, 0'0, NzMcs ),
		delete_trailing( NzMcs, 0'., NpMcs ),
		append( NpMcs, "M", AbrvCs ),
		atom_codes( Abrv, AbrvCs )
		;
		K is Repeats // 1000,
		( K > 0 -> 
			number_codes( K, Kcs ),
			append( Kcs, "K", AbrvCs ),
			atom_codes( Abrv, AbrvCs )
			;
			number_codes( Repeats, RepeatsCs ),
			atom_codes( Abrv, RepeatsCs )
		)
	).
