
:- ensure_loaded( gamma_sf ).

test_gamma :-
	between( 1, 4,  M),
	between( 1, 10, S),
	Iters = 100,
	findall( G, (between(1,Iters,_),gamma(M,S,G)), Gs ),
	sumlist( Gs, Sum ),
	AG is Sum / Iters,
	debug( _, '~w:~w:~w', [M,S,AG] ),
	fail.
