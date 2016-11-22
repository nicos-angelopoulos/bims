is_a_directory( Dir ) :-
	file_exists( Dir ),
	file_property( Dir, type(directory) ).

