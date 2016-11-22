
:- ensure_loaded( library(lists) ).		% append/3, memberchk/2.

:- ensure_loaded( all_dynamic ).			% /1.
:- ensure_loaded( ad_to_slp ).			% /1.

:- ensure_loaded( 'lib/opts_have_help' ).	     % /1.
:- ensure_loaded( 'lib/opts_select_infiles' ).	% /3.

sload( InOpts ) :-
	( opts_have_help(InOpts) ->
		write( 'sload:    load an SLP to memory.' ), nl, nl,
		write( ' Options,' ), nl,
		write( '          rm(RmBool)    remove tmp file, Def: true' ), nl,
		write( '          tmp(Tmp)      temporary file to use' ), nl
		;
		opts_select_infiles( InOpts, Files, OthOpts ),
		sload( Files, OthOpts )
	).

sload( Files, Opts ) :-
	all_dynamic( Files ),
	sload_defaults( Defs ),
	append( Opts, Defs, All ),
	memberchk( rm(Del), All ),
	memberchk( tmp(Tmp), All ),
	ad_to_slp( [rm(Del),tmp(Tmp)] ).

sload_defaults( [rm(true),tmp('tmp_sload.slp')] ).
