:- prolog_load_context( directory, Dir ),
   atom_concat( Dir, '/../..', RelDir ),
   absolute_file_name( RelDir, AbsDir ),
   atom_concat( AbsDir, '/', DirSl ),
   bims_bb_put( mcmcms_top_dir, DirSl ).

% this is not used, maybe it should? here for pack(lib) matching
mcmcms_top_dir( Top )  :-
    bims_bb_get( mcmcms_top_dir, Top ).
