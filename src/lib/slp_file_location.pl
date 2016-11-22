:- multifile( file_search_path/2 ).
:- dynamic( file_search_path/2 ).

:- ensure_loaded( library(system) ).		% working_directory/2.
:- ensure_loaded( non_dir_file_exists).		% /1.

slp_file_location( Name, Source, Slp ) :-
	working_directory( Old, Source ),
	slp_file_location( Name, PrvSlp ),
	working_directory( _S, Old ),
	( (non_dir_file_exists(PrvSlp);Source='.') -> 
		PrvSlp = Slp
		;
		slp_file_location( Name, Slp )
	).

slp_file_location( File, FileName ) :-
	( slp_files_location( File, ".slp",  FileName ) ->
		true
		;
		( slp_files_location( File, ".pl",  FileName ) ->
			true
			;
			( slp_files_location( File, "", FileName ) ->
				true 
				;
				FileName = File
			)
		)
	).

slp_files_location( File, Ext, FileName ) :-
	( file_exists(File) -> 
		FileName = File 
		;
		atom_codes( File, FileCs ),
		append( FileCs, Ext, ExtFileCs ),
		atom_codes( ExtFile, ExtFileCs ),
		( file_exists(ExtFile) ->
			FileName = ExtFile
			;
			( (
			  	append( "slp/", FileCs, DirFileCs ),
				atom_codes( DirFile, DirFileCs ),
				( file_exists(DirFile) ->
					FileName = DirFile
					;
					append( DirFileCs, Ext, SlpDirFileCs ),
					atom_codes( SlpDirFile, SlpDirFileCs ),
					( file_exists(SlpDirFile) ->
					 	FileName = SlpDirFile
						;
						fail
					)
				)) ->		
					true
					;
					file_search_path(slp,Search),
					atom_codes( Search, SearchCs ),
					append( SearchCs, [0'/|FileCs], SeaFileCs ),
					atom_codes( SeaFile, SeaFileCs ),
					( file_exists(SeaFile) ->
						FileName = SeaFile
						;
						append( SeaFileCs, Ext, SeaFileSlpCs ),
						atom_codes( SeaFileSlp, SeaFileSlpCs ),
						( file_exists(SeaFileSlp) -> 
							FileName = SeaFileSlp
							;
							fail
							% FileName = File
						)
					)
			)
		)
	).
