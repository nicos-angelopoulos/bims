term_bn_to_atom( [], [] ).
term_bn_to_atom( [Ch-Pars|T], [ChAt-ParsAts|TAtm] ) :-
	bn_term_to_atom( Ch, ChAt ),
	term_list_to_atoms( Pars, ParsAts ),
	term_bn_to_atom( T, TAtm ).

bn_term_to_atom( Ch, ChAt ) :-
	functor( Ch, Name, Arity ),
	( Arity =:= 0 ->
		ChAt = Ch
		;
		arg( 1, Ch, ArgNum ),
		( number(ArgNum) -> 
			number_codes( ArgNum, ArgCs ),
			atom_codes( Arg, ArgCs )
			;
			Arg = ArgNum
		),
		atom_concat( '_', Arg, UndArg ),
		atom_concat( Name, UndArg, ChAt )
	).

term_list_to_atoms( [], [] ).
term_list_to_atoms( [H|T], [Hat|Tats] ) :-
	bn_term_to_atom( H, Hat ),
	term_list_to_atoms( T, Tats ).
