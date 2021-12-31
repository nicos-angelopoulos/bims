number_atom( Number, Atom ) :-
	( var( Number ) -> 
		atom_codes( Atom, Cs ),
		number_codes( Number, Cs )
		;
		number_codes( Number, Cs ),
		atom_codes( Atom, Cs )
	).
