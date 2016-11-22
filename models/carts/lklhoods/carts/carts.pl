
:- ensure_loaded( 'src/cart_llhood' ).
:- ensure_loaded( '../../src/cart_red_model' ).

lhood_canonical( Cart1, Cart2, Llhood1, Llhood2, Ratio ) :-
	llhood_w_diff( Cart1, Cart2, Llhood1, Llhood2, Diff ),
	( Diff > 0 -> Ratio is 1; Ratio is exp(Diff) ).

% llhood_w_diff( +Cart1, +Cart2, -Llhood1, +Llhood2, Diff ) :-
llhood_w_diff( Cart1, _Cart2, Llhood1, Llhood2, Diff ) :-
	cart_llhood( Cart1, Llhood1 ),
	Diff is Llhood1 - Llhood2.

model_llhood( Cart, Llhood ) :-
	cart_llhood( Cart, Llhood ).
