:- ensure_loaded( library(requires) ).
:- requires( file_to_list_of_lines/2 ).

directory_files( Dir, Files ) :-
	tmp_file( file_directory, Tmp ),
	atom_concat( 'ls -1 ', Dir, LsPfx ),
	atom_concat( LsPfx, ' >& ', LsMfx ),
	atom_concat( LsMfx, Tmp, Ls ),
	shell( Ls ),
	file_to_list_of_lines( Tmp, Lines ),
	atoms_codes_local_swi( Files, Lines ),
	delete_file( Tmp ),
	!.
	
atoms_codes_local_swi( [], [] ).
atoms_codes_local_swi( [A|As], [C|Cs] ) :-
	atom_codes( A, C ),
	atoms_codes_local_swi( As, Cs ).
