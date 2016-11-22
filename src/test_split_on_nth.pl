:- ensure_loaded( kernel ).

test_split_on_nth :-
	Path = [x1,x2,[y1,y2,[z1,z2]],[t1,t2,[w1,w2],t3],x3],
	gen_int( Nth ),
	split_on_nth( Path, Nth, _, _, Left ),
	write( Nth:Left ), nl,
	fail.

gen_int( 1 ).
gen_int( N ) :-
	gen_int( Nwas ),
	( Nwas < 20 -> true ; abort ),
	N is Nwas + 1.
