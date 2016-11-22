:- use_module( library(bims) ).

:- ensure_loaded( library(lists) ).		% memberchk/2.
:- ensure_loaded( library(ordsets) ).		% ord_intersection/4.

:- bims:requires( postfix_atom/2 ).
% :- bims:requires( werr/2 ).
:- bims:requires( read_terms/2 ).
:- bims:requires( current_call/1 ).

:- debug( cart_pop ).

%% cart_populated( +Cr, +Data, -Cart ).
%% cart_populated( +Cr, +Data, -Cart, -FIndices ).
% 
% Cart is equivelant the reduced cart Cr with the idividuals from Data
% populating its branches.
% Data can be a file, a directory or list. If a file it assumes
% to have data/n terms. If a directory, containing a single file of 
% the form *-opts.pl, the argument of the term data_file/1 in this
% file is taken to be pointing to the data file. 
% The list should be of the form [I-FIdx|...], with FIdx = [Fvl-LessOr|_].
% LessOr is the list of data ids (1...N, numbered sequentially from data file
% order) that have a value less or equal to Fvl.
% FIndices is the feature indices corresponding to data, so that
% consequtive calls to cart_populated/4, can reuse them.
%
cart_populated( Cr, Data, Cart ) :-
	cart_populated( Cr, Data, Cart, _ ).

cart_populated( Cr, Data, Cart, _Fids ) :-
	% data_feature_indices( Data, Ids, Fids ),
	data_feature_indices( Data ),
	data:ids(Ids,_),
	debug( cart_pop, 'Ids: ~w', [Ids] ),
	cart_indices_populated( Cr, 1, Ids, Cart ).

cart_indices_populated( l, D, Ids, leaf(D,Ids) ) :- !.
cart_indices_populated( cr(F,Fvl,L,R), D, Ids, cart(F,Fvl,Lfull,Rfull) ) :-
	% memberchk( F-FIndex, Fids ),
	% bims:bb_get( F, FIndex ), 
	data:feature_index( F, FIndex ),
	memberchk( Fvl-PointIds, FIndex ),
	ord_intersection( PointIds, Ids, LIds, RIds ),
	NxtD is D + 1,
	cart_indices_populated( L, NxtD, LIds, Lfull ),
	cart_indices_populated( R, NxtD, RIds, Rfull ).

/*
data_feature_indices( Data, _Ids, Fids ) :-
	Data = [_|_],
	!,
	Data = Fids.
	*/
data_feature_indices( Data ) :-
	exists_file( Data ),
	!,
	data_file_in_module( Data ),
	% bims:bims_ensure_data_loaded( carts, carts, Data, _AbsData ).
	% % read_terms( Data, Terms ),
	% % include( name_term(data), Terms, DTerms ),
	% fixme: also add header recognision
	data_terms_feature_indices.
	% %data_terms_feature_indices( DTerms, Ids, Fids ).
% data_feature_indices( Data, Ids, Fids ) :-
data_feature_indices( Data ) :-
	exists_directory( Data ),
	!,
	debug( cart_pop, 'Dir exists: ~w', Data ),
	directory_files( Data, Entries ),
	include( bims:postfix_atom('-opts.pl'), Entries, OptsFs ),
	OptsFs = [OptsF], % fixme
	directory_file_path( Data, OptsF, OptsP ),
	debug( cart_pop, 'Options path: ~p', OptsP ),
	bims:read_terms( OptsP, Terms ),
	memberchk( data_file(DataF), Terms ),
	debug( cart_pop, 'Options data_file: ~p', DataF ),
	data_feature_indices( DataF ).

data_file_in_module( DataF ) :-
	(   \+ current_module( data )
	  ; \+ current_predicate(data:data_file/1) 
	),
	!,
	data:load_files( DataF, [silent(true)] ),
	assert( data:data_file(DataF) ),
	assert( data:data_models(carts) ).
data_file_in_module( DataF ) :-
	data:data_file( DataF ),
	!.
data_file_in_module( DataF ) :-
	data:data_file( Other ),
	!,
	throw( fixme(data_file_mismatch(Other,DataF)) ).
data_file_in_module( DataF ) :-
	!,
	throw( fixme(data_file_decl_missing(DataF)) ).

data_terms_feature_indices :-
	bims:current_call( data:feature_index(1,_) ),
	!. % fixme: check all requirements are here, not just 
data_terms_feature_indices :-
	once( current_predicate( data:data/Arity ) ),
	functor( Dterm, data, Arity ),
	% functor( Rterm, data, Arity ),
	% retractall( data:Rterm ),
	predicate_property( data:Dterm, number_of_clauses(LenD) ),
	numlist( 1, LenD, Ids ),
	bims:assert_unique( data:ids(Ids,LenD) ),

	NofFeats is Arity - 1,
	functor( Fterm, data, Arity ),
	findall( A-VIPrs, ( between(1,NofFeats,A), 
					arg(A,Fterm,V),
	                    findall( V, data:Fterm, Vals ),
				     sort( Vals, OrdVals ),
				     findall( Val-VIds, ( member(Val,OrdVals),
				                        findall( Id, ( nth1(Id,Vals,IdVal),
												compare(Op,IdVal,Val),
								                    Op\== (>) ),
													IdsUno ),
								    sort( IdsUno, VIds )
								  ), VIPrs 
				  		),
					assert( data:feature_index(A,VIPrs) )
				   ),
		  _Fids ),
	functor( Eterm, data, Arity ),
	arg( Arity, Eterm, AKat ),
	findall( AKat, data:Eterm, AllKats ),
	findall( _, (nth1(I,AllKats,IKat),assert(data:has_category(I,IKat))), _ ),
	sort( AllKats, Kats ),
	bims:assert_unique( data:all_categories(Kats) ),
	length( Kats, K ),
	bims:assert_unique( data:num_of_categories(K) ).

name_term( Name, Term ) :-
	functor( Term, Name, _ ).

/*
red_cart_to_full( CartIn, Depth, InIds, CartOut ) :-
     ( InIds = ids(Ids,_NofIds) ->
          Ord = id_feature
          ;
          Ids = InIds,
          Ord = feature_ids
     ),
     red_cart_to_full( CartIn, Depth, Ord, Ids, CartOut ).

reduced_cart_model( l ) :- !.
reduced_cart_model( cr(_,_,_,_) ).
	*/
