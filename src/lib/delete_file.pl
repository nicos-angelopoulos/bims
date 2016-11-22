:- ensure_loaded( library(lists) ).

delete_file( File ) :-
	exists( File ),
	atom_codes( File, FileCs ),
	append( "rm ", FileCs, RmCs ),
	atom_codes( Rm, RmCs ),
	shell( Rm ).
