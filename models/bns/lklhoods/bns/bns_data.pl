
:- lib(stoics_lib:kv_decompose/3).

bns_data( Opts ) :-
	debug( bims, 'Options for bns_data: ~w', [Opts] ),
	once( data:data(FstVals,_Tms) ),
	length( FstVals, NofVars ),
	numlist( 1, NofVars, Nodes ),
	bims:assert_unique( data:nodes( Nodes ) ),
	findall( Node-Poset, (  member(Node,Nodes),
	                   findall( Val, (data:data(Vals,_Tms2),nth1(Node,Vals,Val))
				    		      , NodeVals ),
				   sort( NodeVals, Poset )
	                ), PosetPrs ),
	stoics_lib:kv_decompose( PosetPrs, _Nds, Posets ),
	sort( Posets, Sosets ),
	bns_data_sets_assert( Sosets, PosetPrs ).

bns_data_sets_assert( [], PosetPrs ) :-
	throw( fixme('is_data_loaded_?_empty_sorted_posets'(PosetPrs)) ).
bns_data_sets_assert( [Set], _Posets ) :-
	!,
	bims:assert_unique( data:possible_values(_,Set) ),
	length( Set, Len ),
	bims:assert_unique( data:domain_size(_,Len) ).
bns_data_sets_assert( [_Set1,_Set2|_], PosetPrs ) :-
	retractall( data:possible_values(_,_) ),
	retractall( data:domain_size(_,_) ),
	findall( _, ( member(Node-Poset,PosetPrs),
			    assert(data:possible_values(Node,Poset)),
			    length(Poset,Polen),
			    assert(data:domain_size(Node,Polen))
	            ), _ ).
