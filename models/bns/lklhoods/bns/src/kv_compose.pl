/** kv_compose( Term1, Term2, KVs ).

Term1 and Term2 are either lists or arbitrary compounds.
KVs is made of -pairs of their values.

==
?- kv_compose( [a,b,c], [1,2,3], Kvs ).

==

@author nicos angelopoulos
@see kv_compose_terms/3

*/
kv_compose( [], [], [] ).
kv_compose( [Hk|Tks], [Hv|Tvs], [Hk-Hv|Tkvs] ) :-
	kv_compose( Tks, Tvs, Tkvs ).
