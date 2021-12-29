
/* NOTE: under current Yap and Swi, High is NEVER generated */

n_distinct_randoms_in( N, Low, High, Rnds ) :-
	( (\+ integer(N);N < 1;N> High - Low) ->   
                                   % recall, High is never generated
		write( user_error, n_distinct_randoms_in/4-aborting ), 
		nl( user_error ),
		abort
		;
		true
	),
	n_distinct_randoms_in_1( N, Low, High, [], Rnds ).

n_distinct_randoms_in_1( 0, _Low, _High, Gen, Rnds ) :-
	!,
	Rnds = Gen.
n_distinct_randoms_in_1( N, Low, High, Gen, Rnds ) :-
	random( Low, High, Rnd ),
	( ord_member(Rnd,Gen) ->
		NxGen = Gen,
		NxN is N
		;
		ord_add_element( Gen, Rnd, NxGen ),
		NxN is N - 1
	),
	n_distinct_randoms_in_1( NxN, Low, High, NxGen, Rnds ).
