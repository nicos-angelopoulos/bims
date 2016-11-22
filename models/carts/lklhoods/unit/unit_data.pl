%
:- ensure_loaded( library(ordsets) ).		% ord_add_element/3.
:- ensure_loaded( library(lists) ).		% member/2.
:- ensure_loaded( bims(src/data) ).          % bims_data_arity/1

% J = number of features.
% T = number of data points.
% C = Categories
% We assume all data points feature all features.
% Data = [[f_1:1,f_1:2,...,f_1:j]-C_1,[f_2:1,f_2:2,...,f_2:J]-C_2,
%           ...,[f_T:1,f_T:2,...,f_T:j]-C_J].
% Feature types: list of J l or n identifiers for qualitative, and quantative
unit_data( Opts ) :-
	data_cart_vacuous( Opts, Vac ),
	bims_data_arity( Arity ),
	% cart_data_feature_types( Arity, FTypes ),
	% debug( bims, 'Data feature types: ~w', [FTypes] ),
	functor( Goal, data, Arity ),
	arg( Arity, Goal, Kat ),
	findall( Kat, data:Goal, Kategories ),
	% all retracts go here so things can be tracked down
	retractall( data:data_file(_) ),
	retractall( data:has_category(_,_) ),
	retractall( data:class_weight(_) ),
	retractall( data:ids(_,_) ),
	retractall( data:all_categories(_) ),
	retractall( data:num_of_categories(_) ),
	retractall( data:knorm(_) ),
	retractall( data:feature_vectors(_) ),
	retractall( data:feature_index(_,_) ),
	retractall( data:splits(_) ),
	retractall( data:vacuous(_) ),
	% bb_put( all_categories, Kats ),
	% bb_put( num_of_categories, K ),
	% bb_put( knorm, LogK ),
	list_to_ord_set( Kategories, Kats ),
	debug( bims, 'Data categories: ~w', [Kats] ),
	length( Kats, K ),
	data_cart_has_category_assert( Kategories, 1, Ids ), 
		               % asserts data:has_category/2 facts.
	data_cart_weights_assert( Kats, Opts ),
	assert( data:vacuous(Vac) ),
	length( Ids, IdsLen ),
	assert( data:ids(Ids,IdsLen) ),
	assert( data:all_categories(Kats) ),
	assert( data:num_of_categories(K) ),
	LogK is lgamma( K ),
	assert( data:knorm(LogK) ),
	%% data_to_split_root( Data, Splits, Ids ) :-
	feature_bb_puts( Arity, 1, Vac, Splits ),
	% cart_data_feature_types_to_possibles( FTypes, Arity, 1, Vac, Psbls ),
	% assert( data:feature_vectors(FeatureVecs) ),
	assert( data:splits(Splits) ).

/*
cart_data_feature_types_to_possibles( [], _Arity, _F, _Vac, [] ).
cart_data_feature_types_to_possibles( [H|T], Arity, F, Vac, [f(F,HPsbls)|TPsbls] ) :-
	% feature_domain( Data, [], Domain, FeatureData, ChpdData ),
	functor( Goal, data, Arity ),
	arg( F, Goal, Fth ),
	findall( Fth, data:Goal, FeatureData ),
	sort( FeatureData, Domain ),
	( H == l ->
		qualitative_domain_to_possibles( Domain, Vac, FeatureData, HPsbls )
		;
		quantitative_domain_to_possibles( Domain, Vac, FeatureData, HPsbls )
	),
	NxF is F + 1,
	cart_data_feature_types_to_possibles( T, Arity, NxF, Vac, TPsbls ).

feature_domain( [], Dmn, Dmn, [], [] ).
feature_domain( [[Hf|TFs]|T], Acc, Dmn, [Hf|MrFs], [TFs|TCData] ) :-
	ord_add_element( Acc, Hf, NxAcc ),
	feature_domain( T, NxAcc, Dmn, MrFs, TCData ).

qualitative_domain_to_possibles( Domain, Vac, FeatureData, Psbls ) :-
	ord_to_subsets( Domain, [], Subsets ),
	qualitative_subsets_to_possibles( Subsets, Vac, FeatureData, Psbls ).

qualitative_subsets_to_possibles( [], _Vac, _FeatureData, [] ).
qualitative_subsets_to_possibles( [H|T], Vac, FeatureData, Psbls ) :-
	qualitative_set_splits( FeatureData, H, 1, Left, Right ),
	length( Left, LeftLen ),
	length( Right, RightLen ),
	( (Vac =< LeftLen, Vac =< RightLen) ->
		Psbls = [s(H,Left,Right)|TPsbls]
		;
		Psbls = TPsbls
	),
	qualitative_subsets_to_possibles( T, Vac, FeatureData, TPsbls ).

qualitative_set_splits( [], _Set, _N, [], [] ).
qualitative_set_splits( [H|T], Set, N, Left, Right ) :-
	NxN is N + 1,
	( ord_member( H, Set ) -> 
		Left = [N|TLeft],
		Right = TRight
		;
		Left = TLeft,
		Right = [N|TRight]
	),
	qualitative_set_splits( T, Set, NxN, TLeft, TRight ).

quantitative_domain_to_possibles( [], _Vac, _FeatureData, [] ).
quantitative_domain_to_possibles( [H|T], Vac, FeatureData, Psbls ) :-
	quantitative_value_splits( FeatureData, H, 1, Left, Right ),
	length( Left, LeftLen ),
	length( Right, RightLen ),
	( (Vac =< LeftLen, Vac =< RightLen) ->
	% ( (not_vacuous(Left),not_vacuous(Right)) -> )
		Psbls = [s(H,Left,Right)|TPsbls]
		;
		Psbls = TPsbls
	),
	quantitative_domain_to_possibles( T, Vac, FeatureData, TPsbls ).

quantitative_value_splits( [], _Val, _N, [], [] ).
quantitative_value_splits( [H|T], Val, N, Left, Right ) :-
	NxN is N + 1,
	( H =< Val -> 
		Left = [N|TLeft],
		Right = TRight
		;
		Left = TLeft,
		Right = [N|TRight]
	),
	quantitative_value_splits( T, Val, NxN, TLeft, TRight ).

% I think this is ok. 
% from Hogg and Tannis p77. we need Unordered sapmling
% without replacement n! / r!(n-r!)  with n = nof objects, r = nof selected.
% then summate over r... for n = 4 , 1 + 4 + 6 + 4 = 15
% | ?- ord_to_subsets( [a,b,c,d], Subs ), length( Subs, Length ).
% Length = 15 ? 
%
ord_to_subsets( [], Subsets, Subsets ).
ord_to_subsets( [H|T], AccSets, Subsets ) :-
	findall( SetH, (member(Set,AccSets),
				 ord_add_element(Set,H,SetH) ), NewSets ),
	append( AccSets, [[H]|NewSets], NxAccSets ),
	ord_to_subsets( T, NxAccSets, Subsets ).
*/

feature_bb_puts( Arity, I, Vac, [I-SplitVs|Tsps] ) :-
	I < Arity,
	!,
	functor( Goal, data, Arity ),
	arg( I, Goal, Ith ),
	findall( Ith, data:Goal, FeatureData ),
	% feature_val_skim( [H|T], Fvs, Rem ),
	sort( FeatureData, UnqSrtFVals ),
	feature_index( UnqSrtFVals, FeatureData, 1, Vac, SplitVs, IthIndex ),
	% bb_put( I, IthIndex ),
	assert( data:feature_index(I,IthIndex) ),
	NxI is I + 1,
	feature_bb_puts( Arity, NxI, Vac, Tsps ).
feature_bb_puts( _Arity, _I, _Vac, [] ).

feature_val_skim( [], [], [] ).
feature_val_skim( [[H|R]|T], [H|TVs], Rem ) :-
	( R == [] -> TRem = Rem ; Rem = [R|TRem] ),
	feature_val_skim( T, TVs, TRem ).

feature_index( [], _Fvs, _Ith, _Vac, [], [] ).
feature_index( [H|T], Fvs, I, Vac, SplitVs, Index ) :-
	val_dichotomy( Fvs, H, 1, LessOrEq, Greater ),
	length( LessOrEq, LeftLen ),
	length( Greater, RightLen ),
	% ( (not_vacuous(LessOrEq),not_vacuous(Greater)) -> )
	( (Vac =< LeftLen, Vac =< RightLen) ->
		SplitVs = [H|TSplitVs],
		Index = [H-LessOrEq|TIndex]
		;
		TSplitVs = SplitVs, TIndex = Index
	),
	NxI is I + 1,
	feature_index( T, Fvs, NxI, Vac, TSplitVs, TIndex ).

val_dichotomy( [], _, _I, [], [] ).
val_dichotomy( [H|T], Pivot, I, LOrE, G ) :-
	( Pivot @< H ->
		TLOrE = LOrE, G = [I|TG]
		;
		LOrE = [I|TLOrE], G = TG
	),
	NxI is I + 1,
	val_dichotomy( T, Pivot, NxI, TLOrE, TG ).


cart_data_feature_types( Arity, FTypes ) :-
	Far is Arity - 1,
	current_predicate( feature_types/Far ),
	functor( Goal, feature_types, Far ),
	call( Goal ),
	!,
	Goal =.. [feature_types|FTypes].
cart_data_feature_types( Arity, FTypes ) :-
	functor( Goal, data, Arity ),
	once( call(data:Goal) ),
	cart_data_feature_derived_types( Arity, 1, Goal, FTypes ).

cart_data_feature_derived_types( 1, _I, _Goal, Types ) :- Types = [].
cart_data_feature_derived_types( B, I, Goal, [T|Types] ) :-
	arg( I, Goal, Ith ),
	cart_data_feature_derived_type( Ith, T ),
	A is B - 1,
	J is I + 1,
	cart_data_feature_derived_types( A, J, Goal, Types ).
	
cart_data_feature_derived_type( Val, Type ) :-
	number( Val ), 
	!,
	Type = n.
cart_data_feature_derived_type( _Val, Type ) :-
	Type = l. % fixme: double check

data_cart_vacuous( Opts, Vac ) :-
	Opts = [Number],
	integer( Number ),
	!,
	Vac is Number.
data_cart_vacuous( _Opts, 5 ).

data_cart_has_category_assert( [], _Id, [] ).
data_cart_has_category_assert( [K|Ks], Id, [Id|TIds] ) :-
	assert( data:has_category( Id, K ) ),
	Nx is Id + 1,
	data_cart_has_category_assert( Ks, Nx, TIds ).

data_cart_weights_assert( Kats, Opts ) :-
	( memberchk(weights(Pairs),Opts) -> true; Pairs = [] ),
	!,
	data_cart_weight_pairs_assert( Pairs, Kats ).

data_cart_weight_pairs_assert( [], Kats ) :-
	findall( _, (member(K,Kats),assert(data:class_weight(K,1))), _ ).
data_cart_weight_pairs_assert( [H|T], Kats ) :-
	data_cart_weight_pair_assert( H, Kats, RemKats ),
	data_cart_weight_pairs_assert( T, RemKats ).

data_cart_weight_pair_assert( K-W, Kats, RemKats ) :-
	!,
	data_cart_weight_category_assert( K, W, Kats, RemKats ).
data_cart_weight_pair_assert( Oth, Kats, Kats ) :-
	debug( bims, 'Ignoring non-paired weight: ~w, please use Kat-Weight', Oth ).

data_cart_weight_category_assert( K, W, Kats, RemKats ) :-
	select( K, Kats, RemKats ),
	!,
	assert( data:class_weight(K,W) ).
data_cart_weight_category_assert( K, _W, Kats, Kats ) :-
	debug( bims, 'Ignoring non-categoried pair: ~w, please use Kat-Weight', K).
