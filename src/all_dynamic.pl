% :- ensure_loaded( 'lib/els' ).

:- dynamic(ad:slp_clause/2).
:- lib(clean_module/1).

% all_dynamic( +File ) :-
% Use term_expansion/2 (defined in ad_expand),
% to consult the predicates of File in module ad.
% All these predicate will also be declared dynamic.
% Finally, retract all current term_expansions (/2).
%
all_dynamic( Files ) :-
	%% write( a_retractall( ad:slp_clause(_,_) ) ), nl,
	retractall( ad:slp_clause(_,_) ),
     %% write( here ), nl,
	% consult( library(ad_expand) ),
    absolute_file_name( pack(bims), BimsRoot, [file_type(directory),access(read)] ),
    directory_file_path( BimsRoot, 'src/lib', BimsLib ),
    directory_file_path( BimsLib, 'ad_expand.pl', AddExpF ),
	% module_requires( bims, ad_expand/0 ),
    load_files( bims:AddExpF, [if(true)] ),
	retractall( ad:s_label(_,_) ),
	retractall( ad:d_label(_,_,_) ),
	retractall( ad_pt:pred_type(_,_) ),
	retractall( pvars:pvars(_,_,_) ),
	retractall( stailr:pvars(_,_,_) ),
	bims_bb_put( ad:all_preds, [] ),
	pl( yap(_), source ),
	pl( swi(_), consult(Files), compile(Files) ),
	% consult( Files ), % when debugging, use this.
	pl( yap(_), no_source ),
	retractall( user:term_expansion(_A,_B) ),
	% bb_put( loaded_slp, Files ),
	% abolish( tmp:_ ),
	clean_module( tmp ),
	!.
