% kv_compose_fixed_v( Keys, Value, KVs ).
kv_compose_fixed_v( [], _Value, [] ).
kv_compose_fixed_v( [Hk|Tks], Value, [Hk-Value|Tkvs] ) :-
	kv_compose_fixed_v( Tks, Value, Tkvs ).
