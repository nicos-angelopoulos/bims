
:- lib(kvs_add_v_to_matching_k/4).

read_term_frequencies( File, Frqs ):-
	open( File, read, In ), 
	read( In, Term ),
	read_term_frqs( Term, In, [], Frqs ),
	close( In ).
	
read_term_frqs( end_of_file, _In, Frqs, Frqs ) :- !.
read_term_frqs( Term, In, Acc, Frqs ) :-
	kvs_add_v_to_matching_k( Acc, Term, 1, NxAcc ),
	read( In, NxTerm ),
	read_term_frqs( NxTerm, In, NxAcc, Frqs ).
