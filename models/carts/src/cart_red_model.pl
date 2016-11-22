to_model( In, In, Out ) :-
	cart_red_model( In, Out ).

cart_red_model( cart(F,Fvl,L,R), cr(F,Fvl,RdL,RdR) ) :-
	!,
	cart_red_model( L, RdL ),
	cart_red_model( R, RdR ).
cart_red_model( leaf(_D,_Ids), l ).
