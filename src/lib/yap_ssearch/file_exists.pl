% :- ensure_loaded( '../flatten' ).
:- ensure_loaded( library(swi) ).

file_exists( File ) :-
	atom_codes( File, FileCs ),
	flatten( ["ls ",FileCs," > /dev/null 2>&1"], LsCs ),
	atom_codes( Ls, LsCs ),
	system(Ls).
