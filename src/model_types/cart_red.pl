:- elm( 'carts/src/lhood' ).            % llhood_w_diff/5, model_llhood/2.
:- elm( 'carts/src/cart_red_model' ).   % /2.
:- bims_bb_put( to_model, true ).

to_model( In, In, Out ) :-
     cart_red_model( In, Out ).
