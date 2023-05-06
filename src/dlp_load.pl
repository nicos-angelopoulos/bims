
:- ensure_loaded( library(lists) ).		% append/3, memberchk/2.

:- ensure_loaded( all_dynamic ).			% /1.
:- ensure_loaded( ad_to_slp ).			% /1.

:- ensure_loaded( 'lib/dlp_file_location' ).	     % /1.

/** dlp_load( DlpF ).
    dlp_load( DlpF, Opts ).

Load a Dlp file into memory. 

The predicate loads two versions of the Dlp file. One in module =|dlp_sld|= (suitalble 
for SLD resolution, see dlp_call/2) and one in module =|dlp_ssr|=, which is suitable for stochastic sampling
resolution (see dlp_sample/1).

Dlp files are looked for in ./dlp and pack(bims/dlp/). So =|dlp_load(coin)|=
will load file pack(bims/dlp/coin.dlp from the local pack(bims) installation.

Opts
  * rm(Rmv=true) 
    whether to remove temporary files which contain the loaded, transformed definite clauses
  * tmp_sld(SldF=DLpF__sld.pl)
    temporary file for the SLD resolution clauses
  * tmp_ssr(SldF=DlpF__ssr.pl)
    temporary file for the stochastic sampling resolution clauses
*/
dlp_load( Dlp ) :-
     dlp_load( Dlp, [] ).

dlp_load( Dlp, Opts ) :-
     %fixme: trace,
     dlp_file_location( Dlp, File ),
	all_dynamic( [File] ),
	dlp_load_defaults( File, Defs ),
	append( Opts, Defs, All ),
	memberchk( rm(Del), All ),
	memberchk( tmp_sld(SldF), All ),
     % fixme:
	ad_to_slp( [msd(sld),rm(Del),tmp(SldF),mod(dlp_sld),ad_clean(false)] ),
	memberchk( tmp_ssr(SsrF), All ),
     ad_to_slp( [msd(rm),rm(Del),tmp(SsrF),mod(dlp_ssr),ad_clean(true)] ).

d_load_defaults( Path, Defs ) :-
     directory_file_path( _, File, Path ), 
     file_name_extension( Stem, _Ext, File ),
     atom_concat( Stem, '__dload__sld', SldStem ),
     file_name_extension( SldStem, pl, SldF ),
     atom_concat( Stem, '__dload__ssr', SsrStem ),
     file_name_extension( SsrStem, pl, SsrF ),
     % SldF = 'tmp_dload__sld.pl',
     % SsrF = 'tmp_dload__ssr.pl'
     Defs = [rm(true),tmp_sld(SldF),tmp_ssr(SsrF)].
