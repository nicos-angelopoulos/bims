% Add to KV a new pair. Key added is assumed to be absent from KV.

% kvs_insert( KVIn, AddK, AddV, KVOut ) :-
kvs_insert( [], AddK, AddV, [AddK-AddV] ).
kvs_insert( [Kin-Vin|KVTail], AddK, AddV, KVOut ) :-
	compare( Op, Kin, AddK ),
	kvs_insert_1( Op, Kin, Vin, KVTail, AddK, AddV, KVOut ).

kvs_insert_1( <, Kin, Vin, KVTail, AddK, AddV, KVOut ) :- 
	!,
	KVOut = [Kin-Vin|KVOutTail],
	kvs_insert( KVTail, AddK, AddV, KVOutTail ).
kvs_insert_1( _, Kin, Vin, KVTail, AddK, AddV, KVOut ) :- 
	KVOut = [AddK-AddV,Kin-Vin|KVTail].
	
