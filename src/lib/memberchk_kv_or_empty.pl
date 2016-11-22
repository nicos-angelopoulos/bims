memberchk_kv_or_empty( Pairs, Key, Value ) :-
	% use cut so this can be used in SLPs.
	memberchk( Key-Value, Pairs ),
	!.
memberchk_kv_or_empty( _Pairs, _Key, [] ).
