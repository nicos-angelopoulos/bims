
non_dir_exists( File ) :-
	pl( swi(_P), 
		( exists_file(File), \+ exists_directory(File) )
		,
          non_dir_exists_non_swi( File )
	).

% Yap doensnt like the \+ in the meta-call when 
% within a module.
non_dir_exists_non_swi( File ) :-
     file_exists( File ),
     \+ file_property( File, type(directory)).
