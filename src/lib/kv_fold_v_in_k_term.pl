kv_fold_v_in_k_term( [], [] ).
kv_fold_v_in_k_term( [K-V|T], [KV|R] ) :-
	K =.. [Name|KArgs],
	append( KArgs, [V], KVArgs ),
	KV =.. [Name|KVArgs],
	kv_fold_v_in_k_term( T, R ).
