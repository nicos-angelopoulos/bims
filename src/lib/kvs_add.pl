kvs_add( [], List2, List2 ) :- !.
kvs_add( List1, [], List1 ).
kvs_add( [K1-V1|T1], [K2-V2|T2], [K3-V3|T3] ) :-
	( K1==K2 -> 
		V3 is V1 + V2, K3 = K1,
		L1 = T1, L2 = T2
		;
		( K1 @< K2 -> 
			V3 = V1, K3 = K1,
			L1 = T1, L2 = [K2-V2|T2]
			;
			V3 = V2, K3 = K2,
			L1 = [K1-V1|T1], L2 = T2
		)
	),
	kvs_add( L1, L2, T3 ).

