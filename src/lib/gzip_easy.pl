:- ensure_loaded( library(sys_info) ).		% /1.

gzip_easy( File ) :-
	( atom_concat( Stem, '.gz', File ) ->
		atom_concat( 'gzip ', Stem, Gzip ),
		sys_info( Gzip )
		;
		true
	).
