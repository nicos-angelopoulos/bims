:- prolog_load_context( directory, Dir ),
   atom_concat( Dir, '/../..', RelDir ),
   absolute_file_name( RelDir, AbsDir ),
   atom_concat( AbsDir, '/', DirSl ),
   bims_bb_put( mcmcms_top_dir, DirSl ).
