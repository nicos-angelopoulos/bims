:- ensure_loaded( library(lists) ).		% reverse/2.

kv_decompose_on_max_v( [Hk-Hv|T], MaxVKs, MaxV ) :-
	kv_decompose_on_max_v_1( T, Hv, [Hk], RevKs, MaxV ),
	reverse( RevKs, MaxVKs ).

kv_decompose_on_max_v_1( [], CurMaxV, MaxVKs, MaxVKs, CurMaxV ).
kv_decompose_on_max_v_1( [Hk-Hv|T], CurMaxV, Acc, RevKs, MaxV ) :-
	( Hv > CurMaxV -> 
		NxCurMaxV is Hv, NxAcc = [Hk]
		;
		NxCurMaxV is CurMaxV,
		( Hv =:= CurMaxV ->
			NxAcc = [Hk|Acc]
			;
			NxAcc = Acc
		)
	),
	kv_decompose_on_max_v_1( T, NxCurMaxV, NxAcc, RevKs, MaxV ).
