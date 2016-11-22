:- ensure_loaded( library(lists) ).	% select/3.

kv_additive_consolidation( [], [] ).
kv_additive_consolidation( [K-V|T], Consol ) :-
	( select(K-V1,T,ResT) ->
		V2 is V1 + V,
		NxList = [K-V2|ResT],
		Consol = TConsol
		% kv_additive_consolidation( [K-V2|ResT], Consol )
		;
		Consol = [K-V|TConsol],
		NxList = T
		% kv_additive_consolidation( T, Tconsol )
	),
	kv_additive_consolidation( NxList, TConsol ).
