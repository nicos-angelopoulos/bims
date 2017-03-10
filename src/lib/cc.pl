% :- dynamic cc/1.
cc.

init_cc :-
	% retractall( cc(_) ),
	bims_bb_put( cc, 1 ).

next_cc( CC ) :- 
	NxtCC is CC + 1,
	% retractall( cc(_) ),
	% assert( cc(NxtCC) ).
	bims_bb_put( cc, NxtCC ).

:- ( bims_bb_get(cc,_) -> true ; init_cc ).
% :- ( cc(_CC) -> true ; init_cc ).
