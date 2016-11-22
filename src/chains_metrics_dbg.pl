:- ensure_loaded( '../carts/auxil/report/' ). % get_cart_s_nof_leaves_and_max_depth/4.

chains_metrics_dbg( Cold, Hots ) :-
	get_cart_s_nof_leaves_and_max_depth( Cold, 0, ColdLvs, ColdDpts ),
	hot_chains_metrics_get( Hots, HotLvs, HotDpts ),
	dbg( 937, l(ColdLvs,HotLvs) ).
	dbg( 937, d(ColdDpt,HotDpts) ).

hot_chains_metrics_get( [], [], [] ).
hot_chains_metrics_get( [H|T], [HLvs|TLvs], [HDpt|TDpts] ) :-
	H = hc(MODh,_LLh1,_Path1,_Sop1,_LgS1,_Pow1),
	get_cart_s_nof_leaves_and_max_depth( MODh, 0, HLvs, HDpt ),
	hot_chains_metrics_get( T, TLvs, TDpts ).
