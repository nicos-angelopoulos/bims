
:- ensure_loaded( library(lists) ).		% append/3, memberchk/2.

:- ensure_loaded( all_dynamic ).			% /1.
:- ensure_loaded( ad_to_slp ).			% /1.

:- ensure_loaded( 'lib/opts_have_help' ).	     % /1.
:- ensure_loaded( 'lib/opts_select_infiles' ).	% /3.
:- ensure_loaded( 'lib/dlp_file_location' ).	     % /1.

% fixme: sload(coin).

sload( InOptsPrv ) :-
     ( is_list(InOptsPrv) -> InOpts=InOptsPrv; InOpts=[InOptsPrv] ),
	( opts_have_help(InOpts) ->
		write( 'sload:    load an SLP to memory.' ), nl, nl,
		write( ' Options,' ), nl,
		write( '          rm(RmBool)    remove tmp file, Def: true' ), nl,
		write( '          tmp(Tmp)      temporary file to use' ), nl
		;
		opts_select_infiles( InOpts, Files, OthOpts ),
		sload( Files, OthOpts )
	).

sload( FilesIn, Opts ) :-
     %fixme: trace,
     maplist( dlp_file_location, FilesIn, Files ),
	all_dynamic( Files ),
	sload_defaults( Files, Defs ),
	append( Opts, Defs, All ),
	memberchk( rm(Del), All ),
	memberchk( tmp(Tmp), All ),
     % fixme:
	ad_to_slp( [msd(sld),rm(Del),tmp(Tmp)] ).

% sload_defaults( [rm(true),tmp('tmp_sload.slp')] ).
sload_defaults( Files, Defs ) :-
     ( Files = [Path] -> 
          directory_file_path( _, File, Path ), 
          file_name_extension( Stem, _Ext, File ),
          atom_concat( Stem, '__dload', Dtem ),
          file_name_extension( Dtem, pl, TmpF )
          ;
          TmpF = 'tmp_dload.pl'
     ),
     Defs = [rm(true),tmp(TmpF)] ).
