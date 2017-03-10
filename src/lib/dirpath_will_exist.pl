
:- ensure_loaded( library(dirpath_first_dir_and) ).	% /3.

dirpath_will_exist( DirPath ) :-
	working_directory( Origin, Origin ),
	( dirpath_will_exist_1( DirPath ) ->
		true
		;
		true
	),
	working_directory( _DirPathThere, Origin ).

dirpath_will_exist_1( DirPath ) :-
	( (DirPath=='';DirPath=='/') ->
		true
		;
		( dirpath_first_dir_and( DirPath, Head, Tail ) ->
			dirpath_will_exist_progress( Head ),
			working_directory( _Origin, Head ),
			dirpath_will_exist_1( Tail )
			;
			dirpath_will_exist_progress( DirPath )
		)
	).

dirpath_will_exist_progress( Head ) :-
	( exists_directory(Head) ->
			true
			;
               ( file_exists(Head) ->
			     working_directory( This, This ),
			     Type = not_a_directory,
			     dirpath_will_exist_error_dtype( This, Head, Type ),
			     fail
		          ;
		          make_directory( Head )
               )
	).

dirpath_will_exist_error_dtype( Current, Proposed, ExistType ) :-
	write( user_error, 'Cannot create dir, ' ),
	write( user_error, Proposed ), 
	write( user_error, ', in dir, ' ), 
	write( user_error, Current ),
	write( user_error, ',\n because file of type ' ),
	write( user_error, ExistType ), 
	write( user_error, ' already exists with the same name. ' ),
	nl( user_error ).

