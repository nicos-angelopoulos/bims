kvs_add_v_to_matching_k( [], K, V, [K-V] ).
kvs_add_v_to_matching_k( [Hk-Hv|T], K, V, All ) :-
	compare( Op, Hk, K ),
	kvs_add_v_to_matching_k_1( Op, Hk, Hv, T, K, V, All ).

kvs_add_v_to_matching_k_1( =, Hk, Hv, T, _K, V, All ) :-
	Va is Hv + V,
	All = [Hk-Va|T].
kvs_add_v_to_matching_k_1( >, Hk, Hv, T, K, V, All ) :-
	All = [K-V,Hk-Hv|T].
kvs_add_v_to_matching_k_1( <, Hk, Hv, T, K, V, All ) :-
	All = [Hk-Hv|Ta],
	kvs_add_v_to_matching_k( T, K, V, Ta ).
