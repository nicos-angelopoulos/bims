% requires bb_get( mcmcms_top_dir, Dir ).

elm_location( Src, File ) :-
	bims_bb_get( mcmcms_top_dir, Dir ),
	atom_concat( Dir, Src, File ).
