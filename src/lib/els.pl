els( File ) :-
	bims_bb_put( no_slp_expansions, true ),
	ensure_loaded( File ),
	% compile( File ),
	bims_bb_delete( no_slp_expansions, _ ).

els_starts :- bims_bb_put( no_slp_expansions, true ).
els_ends   :- bims_bb_delete( no_slp_expansions, _ ).
