
mktemp_in_dir( Tmpl, Dir, Fname ) :-
	% pl( sicstus(_S), absolute_file_name( Dir, AbsDir )),
	% pl( yap(_Y),  Dir = AbsDir ),
	% pl( swi(_P), absolute_file_name( Dir, AbsDir )),
     absolute_file_name( Dir, AbsDir ),
	( exists_directory(AbsDir) ->
		atom_codes( Tmpl, TmplCs ),
		atom_codes( AbsDir, AbsDirCs ),
		flatten( [AbsDirCs,"/",TmplCs], DirTemplCs ),
		atom_codes( DirTempl, DirTemplCs ),
		mktemp( DirTempl, Fname )
		;
		mktemp( Tmpl, Fname )
	).
