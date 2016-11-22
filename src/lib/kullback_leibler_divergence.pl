kullback_leibler_divergence( [H1|T1], L2, Sum ) :-
	kullback_leibler_divergence_1( [H1|T1], L2, 0, _, Sum ).

kullback_leibler_divergence_1( [], [], Sum, [], Sum ).
kullback_leibler_divergence_1( [H1|T1], [H2|T2], Acc, [Hc|Tcs], Sum ) :-
	Hc is ( H1 * log((H1 / H2))),
	NxAcc is Acc + Hc,
	kullback_leibler_divergence_1( T1, T2, NxAcc, Tcs, Sum ).
