std_afn( Alias, AbsFileName ) :-
	std_afn_rec( Alias, AbsFileNameCs ),
	atom_codes( AbsFileName, AbsFileNameCs ).

std_afn_rec( Alias, AbsFileNameCs ) :-
	( Alias =.. [Fun,Arg] ->
		std_afn_rec( Arg, NestCs ),
		( file_search_path(Fun,Path) ->
			true
			;
			Path = Fun
			% this is my addition, not part of the
			% ``standard''
		),
		atom_codes( Path, PathCs ),
		append( PathCs, [0'/|NestCs], AbsFileNameCs )
		;
		atom_codes( Alias, AbsFileNameCs )
	).

