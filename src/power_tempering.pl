:- ensure_loaded( library(nth_replace) ).		% /5.

power_backtrack( [], _Bid, _Bp, _SGl, [] ).
power_backtrack( [H|T], Bid, Bp, SGl, [HBck|TBcks] ) :-
	H = hc(MODi/MODiKp,LLhi,Pathi,Sopi,LgSi,Pow),
	backtrack_on_nxt( Bid, Bp, Pathi, MODi/MODiKp, SGl, Sopi, LgSi, Pathst, Sopst, LgSst, MODst/MODstKp, _Nth, BCntr ),
	( user:lhood_canonical( MODst, MODi, LLhst, LLhi, Ratio ) ->
		( bims_bb_get(gpr,true) -> 
			global_priors_ratio( MODst, MODi, GlobalPsR ),
			Alpha is min( 1, (BCntr * (Ratio ** Pow) * GlobalPsR))
			% Alpha is min( 1, (BCntr * exp(Ratio,Pow) * GlobalPsR))
			;
			Alpha is min( 1, (BCntr * (Ratio ** Pow)))
			% Alpha is min( 1, (BCntr * exp(Ratio,Pow)))
		)
		; 
		werr( [['Likelihood call failure, for: ',lhood_canonical(MODst,MODi,LLhst, LLhi, Ratio)],['Aborting...']] ),
		abort
	),
	on_random( Alpha, jump, dont_jump, _Rnd, _Whc, Jump ), 
	( Jump == jump -> 
		HBck = hc(MODst/MODstKp,LLhst,Pathst,Sopst,LgSst,Pow)
		;
		HBck = H 
	),
	power_backtrack( T, Bid, Bp, SGl, TBcks ).

power_chain_swap( Fst, Sec, InHotChns, Jump, OutHotChns ) :-
	nth_replace( Fst, InHotChns, NwChn1, Chn1, FstHotChns ),
	nth_replace( Sec, FstHotChns, NwChn2, Chn2, OutHotChns ),
	Chn1 = hc(MOD1/MOD1Kp,LLh1,Path1,Sop1,LgS1,Pow1),
	Chn2 = hc(MOD2/MOD2Kp,LLh2,Path2,Sop2,LgS2,Pow2),
	lhood_canonical( MOD2, MOD1, _, LLh1, Ratio ),
	Alpha is min(1,(Ratio ** (Pow1-Pow2))),
	on_random( Alpha, jump, dont_jump, _Rnd, _Whc, Jump ),
	( Jump == jump -> 
		NwChn1 = hc(MOD2/MOD2Kp,LLh2,Path2,Sop2,LgS2,Pow1),
		NwChn2 = hc(MOD1/MOD1Kp,LLh1,Path1,Sop1,LgS1,Pow2)
		% user:display_model( MOD1, [title('was: Mod 1'),lhood(LLh1)] ),
		% user:display_model( MOD2, [title('was: Mod 2'),lhood(LLh2)] ),
		% bb_put( power_jump, true )
		;
		NwChn1 = hc(MOD1/MOD1Kp,LLh1,Path1,Sop1,LgS1,Pow1),
		NwChn2 = hc(MOD2/MOD2Kp,LLh2,Path2,Sop2,LgS2,Pow2)
	).
	% werr( [[Fst-Sec-Jump-a(Alpha)-p1(Pow1)-p2(Pow2)-r(Ratio)]] ).
