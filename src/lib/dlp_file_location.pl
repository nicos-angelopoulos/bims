:- ensure_loaded( library(filesex) ). % directory_file_path/3.

% 23.05.06, new attempt, (there was an old one) copied over from pack(pepl), and then slightly modified

/*
slp_data_file_location( File, FileName ) :-
	slp_files_location( File, ".pl",  FileName ).
*/

dlp_file_location( File, FileName ) :-
	( dlp_files_location( File, dlp,  FileName ) ->
		true
		;
		( dlp_files_location( File, pl,  FileName ) ->
			true
			;
			( dlp_files_location( File, '', FileName ) ->
				true 
				;
				FileName = File
			)
		)
	).

dlp_files_location( File, Ext, Filename ) :-
	( file_or_ext_exists(File,Ext,Filename) ->
		true
		;
		( ( directory_file_path(dlp,File,DirFile),
		    file_or_ext_exists(DirFile,Ext,Filename) ) ->
				true
				;
				( (absolute_file_name(dlp(File),Dlp),
				   file_or_ext_exists(Dlp,Ext,Filename) ) ->
						true
						;
						directory_file_path( 'bims/dlp', File, Pfile ),
						Opts4 = [access(read),extensions(['',dlp])],
						( (absolute_file_name(pack(Pfile),Pack,Opts4),
				             file_or_ext_exists(Pack,Ext,Filename) ) ->
								true
								;
								false
						)
				)
		)
	).

file_or_ext_exists( File, _Ext, Filename ) :-
	exists_file( File ),
	!,
	Filename = File.
file_or_ext_exists( File, Ext, Filename ) :-
	file_name_extension( File, Ext, Filename ),
	exists_file( Filename ).
