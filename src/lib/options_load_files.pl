:- ensure_loaded( to_list ).

options_load_files( Opts ) :-
	findall( F, 
				( member( load(F), Opts ),
				  load_files( F ) ),
				_ ),
	findall( L, 
				( member( load(L,LoptsEr), Opts ),
				  to_list( LoptsEr, Lopts ),
				  load_files( L, Lopts ) ),
				_ ).
