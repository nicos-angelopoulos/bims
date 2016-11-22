:- ensure_loaded( library(sys_info) ).		% /1.

gunzip_easy( File, Stem ) :-
	( atom_concat( Stem, '.gz', File ) ->
		atom_concat( 'gunzip ', File, Gunzip ),
		sys_info( Gunzip )
		;
		File = Stem
	).
