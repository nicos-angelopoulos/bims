:- ensure_loaded( library(lists) ). 	% memberchk/2.
:- ensure_loaded( gzip_files ). 		% /1.

options_gzip_at_end( Opts, Fs, Gz ) :-
	( memberchk(gz(Gz),Opts) -> 
		( Gz == false ->
			true
			;
			( Gz == true -> 
				gzip_files( Fs )
				;
				write( user_error, 'Cannot understand value: ' ),
				write( user_error, Gz ),
				write( user_error, ' in argument gz/1. ' ), 
				nl( user_error ),
				write( user_error, 'No gzipping will take place.' ),
				nl( user_error )
			)
		)
		;
		gzip_files( Gz )
	).
