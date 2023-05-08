% :- dynamic( n_table/3 ).
% :- ensure_loaded( set_class_weights ).
% :- ensure_loaded( '../../src/init_lib' ).

% :- requires( bims:kv_compose_fixed_v/3 ).
:- bims_re_load(kv_compose_fixed_v).

% These assume Alpha_i = 1.
% We follow, Eq. 16, Bayesian Cart Model Search, Chipman et al 
%
cart_llhood( Cart, Llhood ) :-
	cart_llhood_1( Cart, Leaves, Sum ),
	data:knorm( Knorm ),
	Llhood is (Leaves * Knorm) + Sum.

cart_llhood_1( cart(_F,_S,L,R), Lvs, Llhood ) :-
	cart_llhood_1( L, LLvs, LLlhood ),
	cart_llhood_1( R, RLvs, RLlhood ),
	Lvs is LLvs + RLvs,
	Llhood is LLlhood + RLlhood.
cart_llhood_1( leaf(_D,LDIds), 1, Mterm ) :-
	data:all_categories( Kats ),
	data:num_of_categories( K ),
	kv_compose_fixed_v( Kats, 1, KatPrs ),
	sum_of_lgammas_n_ik_v2( LDIds, KatPrs, 0, Ni, LgGofNikS ),
	NiAddK is Ni + K,
		/* 	logamma( NiAddK, LgGofNiAddK ), 
			Mterm is LgGofNikS - LgGofNiAddK. */
	Mterm is LgGofNikS - lgamma(NiAddK).

sum_of_lgammas_n_ik_v2( [], KatPrs, Ni, Ni, LgGofNikS ) :-
	kv_sum_logamma_of_vs( KatPrs, 0, LgGofNikS ).
sum_of_lgammas_n_ik_v2( [IdLow-IdHgh|T], AccKatPrs, AccNi, Ni, LgGofNikS ) :-
     !,
     IdLim is IdHgh + 1,
     ( IdLow > IdHgh -> throw(funny_range_in_cart_lhood(IdLow,IdHgh)) ; true ),
     sum_of_lgammas_n_ik_v2_range( IdLow, IdLim, AccKatPrs, AccNi, NxKatPrs, NxNi ),
     sum_of_lgammas_n_ik_v2( T, NxKatPrs, NxNi, Ni, LgGofNikS ).
sum_of_lgammas_n_ik_v2( [Id|T], AccKatPrs, AccNi, Ni, LgGofNikS ) :-
     data:has_category( Id, Kat ),
     data:class_weight( Kat, W ),
     NxNi is AccNi + W,
     kvs_add_n_to_v( AccKatPrs, Kat, W, NxKatPrs ),
     sum_of_lgammas_n_ik_v2( T, NxKatPrs, NxNi, Ni, LgGofNikS ).

sum_of_lgammas_n_ik_v2_range( IdLim, IdLim, AccKatPrs, AccNi, KatPrs, Ni ) :-
     !,
     KatPrs = AccKatPrs,
     Ni     = AccNi.
sum_of_lgammas_n_ik_v2_range( IdLow, IdLim, AccKatPrs, AccNi, KatPrs, Ni ) :-
     has_category( IdLow, Kat ),
     ( class_weight(Kat,W) -> true; W is 1 ),
     NxNi is AccNi + W,
     kvs_add_n_to_v( AccKatPrs, Kat, W, NxKatPrs ),
     NxIdLow is IdLow + 1,
     sum_of_lgammas_n_ik_v2_range( NxIdLow, IdLim, NxKatPrs, NxNi, KatPrs, Ni ).

% copied from kvs_add_one_to_v/3 below.
kvs_add_n_to_v( [Hk-Hv|T], K, N, All ) :-
	Hk @< K,
	!,
	All = [Hk-Hv|Ta],
	kvs_add_n_to_v( T, K, N, Ta ).
kvs_add_n_to_v( [Hk-Hv|T], _K, N, [Ha|Ta] ) :-
	Va is Hv + N,
	Ha = Hk-Va,
	Ta = T.

kv_sum_logamma_of_vs( [], Sum, Sum ).
kv_sum_logamma_of_vs( [_K-Tms|T], Acc, Sum ) :-
	/*  logamma( Tms, LgG ),
	    NxAcc is Acc + LgG, */
	NxAcc is Acc + lgamma(Tms),
	kv_sum_logamma_of_vs( T, NxAcc, Sum ).
