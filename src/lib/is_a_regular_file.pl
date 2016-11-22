:- pl( swi(_), true, ensure_loaded(library(system)) ).

is_a_regular_file( File ) :-
	pl( swi(_), exists_file(File), 
			(file_exists(File),file_property(File,type(regular)))
		).
