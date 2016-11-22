% requires bb_get( mcmcms_top_dir, Dir ).

elm( Src ) :-
	bims_bb_get( mcmcms_top_dir, Dir ),
     ( atomic(Src) -> Src=SrcAtom; term_to_atom(Src,SrcAtom) ),
	atom_concat( Dir, SrcAtom, File ),
	ensure_loaded( File ).
