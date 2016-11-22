:- ensure_loaded( init_lib ).

:- ensure_loaded( kernel_nxt ).
:- ensure_loaded( library(kvs_add_v_to_matching_k) ).

path( [1,3,4/0.5,2] ).
path( [1,3,4/0.5,[6/0.2],[],2] ).
path( [1,3,4/0.5,[6/0.2],[5,5,4/0.3,1,[1,6/0.4]],2,7] ).
path( [1,3,4/0.5,[6/0.2],[5,5,4/0.3,1,[9,9,9],[1,6/0.4]],2,7] ).
path1( [1,3,4/0.5,[6/0.2],[5,5,4/0.3,1,[9,9,9],[1,6/0.4]],2,7] ).

test_nxt0 :-
	path( Path ),
	length( Path, Lgt ),
	( Lgt =:= 70 -> trace ; true ),
	write( path(Path) ), nl, 
	rec_cps( Path, 1, 0, [], 0, 0, [], FiN, Cps ),
	write( fin(FiN) ), nl,
	write( Cps ), nl, nl,
	fail.
test_nxt0.

% test_nxt :-
	
test_nxt :-
	path1( Path ), 
	rec_cps( Path, 1, 0, [], 0, 0, [], FiN, Cps ),
	PossSel is FiN - 1,
	n_counters( PossSel, PossSel, Counters ), 
	test_nxt( 10000, Cps, Counters, Final ),
	write( Final ), nl.

test_nxt( 0, _Cps, Cnts, Cnts ) :- !.
test_nxt( I, Cps, InCnts, Cnts ) :-
	I > 0,
	bu_select_bp( Cps, 0.5, Sel-_Prb ),
	kvs_add_v_to_matching_k( InCnts, Sel, 1, NxtCnts ),
	NxI is I - 1,
	test_nxt( NxI, Cps, NxtCnts, Cnts ).
	
n_counters( 0, _, [] ) :- !.
n_counters( I, Max, [H-0|T] ) :-
	H is Max - I + 1,
	NxI is I - 1,
	n_counters( NxI, Max, T ).
