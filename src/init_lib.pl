:- multifile library_directory/1.
:- dynamic library_directory/1.

:- use_module( library(system) ). % Yap directory_files

assert_model_paths :-
	AbsOpts = [file_type(directory),access(exist),solutions(all)],
	absolute_file_name( bims(models), ModsDir, AbsOpts ),
	directory_files( ModsDir, SubEntries ),
	findall( SubSrc, ( member(SubE,SubEntries),
				 \+ atom_concat( '.', _, SubE ),
				 directory_file_path(ModsDir,SubE,Path),
				 directory_file_path(Path,src,SubSrc),
				 exists_directory(SubSrc),
				 debug(bims,'Asserting model path:~w, at:~w',[SubE,SubSrc] ),
			      assert(user:file_search_path(SubE,SubSrc))
			), _Subs ),
	!.
assert_model_paths :
	debug( bims, 'Failed to assert any model paths', [] ).

:- prolog_load_context( directory, Here ),
   directory_file_path( Top, src, Here ),
   debug( bims, 'Asserting: ~w', Top ),
   assert( user:file_search_path(bims,Top) ),
   atom_concat( Here, '/', HereSl ),
   atom_concat( HereSl, 'lib/on_pl', Pl ),
   ensure_loaded( Pl ),
   assert_model_paths.

:- pl( swi(_A),  ensure_loaded('lib/swi_compat') ).
% :- ensure_loaded( 'lib/requires_minimal' ). % requires/1.
:- ensure_loaded( 'lib/defines' ).
:- ensure_loaded( 'lib/els' ).
:- ensure_loaded( 'lib/mcmcms_top_dir' ).
:- ensure_loaded( 'lib/elm' ).

/* now defined in library(requires)
lib( Lib ) :- 
	ensure_loaded( library(Lib) ).
	*/
:- (current_predicate(lib/1)->true; 
       assert(lib(Lib):-use_module(library(Lib)))
	).

yap_start :-
	use_module( library(lists) ),
	getcwd( D ),
	atom_codes( D, DCs ),
	append( DCs, "/lib", LibCs ),
	atom_codes( Lib, LibCs ),
	assert_lib_dir_if( Lib ),
     % a little bit of a hack, in the long run ask Vitor to change
     % the behaviour of Yap
     assert( portray_message( _X, defined_elsewhere((tmp: :: /2),_Where) ) ),
     set_prolog_flag( unknown, error ),
	set_prolog_flag( single_var_warnings, on ),
	set_prolog_flag( redefine_warnings, on ),
	set_prolog_flag( informational_messages, off ).

sicstus_start :-
	absolute_file_name( 'lib/', Lib ),
	assert_lib_dir_if( Lib ),
	pl_version( sicstus, (3:9:0), NoP=sics38_pncms, NoP=sics39_pncms ),
	pl( sicstus(3:8:_), ensure_loaded(library(sics38_pncms)) ),
	pl( [sicstus(3:9:_),sicstus(3:10:_),sicstus(3:11:_),sicstus(3:12:0),
	     sicstus(3:12:1)],
		ensure_loaded( library(sics39_pncms) ) ),
	pl_version( sicstus, (3:12:2), true, set_prolog_flag(informational,off) ).

/*
swi_start :-
        % absolute_file_name( 'lib/', Lib ),
        prolog_load_context( directory, Dir ),
        atom_concat( Dir, '/lib', Lib ),
        % write( lib(Lib) ), nl,
        assert_lib_dir_if( Lib ).
	   */
swi_start.

assert_lib_dir_if( Lib ) :-
	( ( current_predicate( library_directory, library_directory(_) ),
          % in the long run we would like to remove user: below
          user:library_directory(Lib)) -> 
		true 
		; 
		assertz( user:library_directory(Lib) ) 
	).

mcmcms_maintainer( 'nicos.angelopoulos@gmail.com' ).

:- pl( sicstus(_A), sicstus_start ).
:- pl( yap(_A), yap_start ).
:- pl( swi(_A), swi_start ).
:- prolog_load_context( directory, Dir ),
   atom_concat( Dir, '/', DirSl ),
   bims_bb_put( mcmcms_src_dir, DirSl ).
