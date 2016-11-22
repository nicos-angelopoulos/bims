
:- ensure_loaded( bims(models/carts/src/cart_red_model) ).

lhood_canonical( _, _, 0, 0, 1 ).

model_llhood( _, 0 ).
