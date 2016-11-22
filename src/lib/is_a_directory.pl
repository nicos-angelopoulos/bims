:- pl( swi(_), true, ensure_loaded( library(system)) ).

is_a_directory( Dir ) :-
	pl( swi(_), exists_directory(Dir), 
				(
				  file_exists(Dir),
				  file_property(Dir,type(directory))
				)
		).
