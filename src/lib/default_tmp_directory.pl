:- ensure_loaded( library(system) ).		% environ/2.

default_tmp_directory( Dir ) :-
	( ( current_predicate(file_search_path/2),
		file_search_path(tmp,Dir) )  ->
		true
		;
		( environ( 'PL_TMP', Dir ) -> 
			true
			;
			environ( 'TMP', Dir ) ->
			true
			;
			Dir = '.'
		)
	).
