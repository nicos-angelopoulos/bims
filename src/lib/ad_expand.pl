
:- op( 550, yfx, :: ).
:- op( 550, yfx, ## ).
% :- op( 1100, xfy, ^ ).

:- use_module( library(lists) ).		% nth/3.
:- requires( head_to_spec/2 ).
:- requires( ord_only_add_elem/3 ).
:- requires( to_list/2 ).

:- dynamic user:term_expansion/2.
:- dynamic ad:s_label/2.
:- dynamic ad_pt:pred_type/2.

:- dynamic pvars:pvars/3.

:- bims_bb_put( ad:all_preds, [] ).

s_tail_recursive( Spec ) :-
	assert( stailr:stailr(Spec) ).

s_no_bpoint( Spec ) :-
	assert( stailr:lv_no_bpoint(Spec) ).

bims:pvars( Head, VarCallPairs ) :-
	Head =.. [Name|Args],
	length( Args, Arity ), 
	Spec = Name/Arity,
	( pvars:pvars( Spec, SmArgs, SmPairs ) ->
			Goal = Spec, Cul1 = SmArgs-SmPairs, Cul2 = Args-VarCallPairs,
			Mess = 'predicate already has pvars defined',
			Error = consistency_error(Goal,Cul1,Cul2,Mess),
			print_message( error, Error )
			;
			assert( pvars:pvars(Spec,Args,VarCallPairs) )
	).

% Types = [ ns-non_stochastic, s-stochastic, d-distributional],
bims:already_defined( Spec, ThisType, Types, Cul2 ) :-
	ad_pt:pred_type( Spec, DefinedType ),
	DefinedType \== ThisType, % currently this is always true
	member( DefinedType-FullType, Types ),
	Cul2 =.. [FullType,Spec],
	!.
bims:already_defined( Spec, ThisType, Types, Cul2 ) :-
	bims:nth( N, Types, ThisType-_FullType ),
	bims:nth( I, Types, Ith-IthType ),
	I =:= N,
	Spec = (Name/Arity),
	ConfArity is Arity + I - N,
	ad_pt:pred_type( (Name/ConfArity), Ith ),
	Cul2 =.. [IthType,Spec],
	!.

pred_dfnd_err( Goal, Cul1, Cul2 ) :-
	Mess = ' predicate, or conflicting one, already defined ',
	print_message( error, consistency_error(Goal,Cul1,Cul2,Mess) ).

bims:term_expansion( Term, Expant ) :-
	% write( user_error, term(Term) ), nl( user_error ),
	\+ bims:bims_bb_get( no_slp_expansions, true ),
	Term \== end_of_file,
	Term \= (?-_An1),
	Term \= (:-_An2),
	( Term = (InH :- Body) ->
		BodyFlag = present
		; 
		InH = Term
	),
	( InH = (InL::Head) -> 
		( InL = (LEx::InLVs) ->
			bims:to_list( InLVs, LVs ),
			Stochastic = d
			;
			Label is InL,
			Stochastic = y
		)
		% ( BodyFlag == present -> 
			% Clause = (Head :- Body)
			% ;
			% Clause = Head
		% )
		;
		Stochastic = n,
		Head = InH
		% ( BodyFlag == present -> 
			% Clause = Term,
			% Head = InH
			% ;
			% Clause = 
			% Head = Term
		% )
	),
	Mod = ad,
	% current_module( Cmod ), 
	( BodyFlag == present -> 
		( Stochastic = d ->
			OutBody = (LVs,Body)
			;
			OutBody = Body
		)
		% true 
		;
		OutBody = true
	),
	TmpClause = (Head:-OutBody),
	bims:head_to_spec( Head, Spec ),
	% pl( sicstus(_S), Expant = [(:- dynamic(Mod:Spec)),Mod:OutClause] ),
	% pl( yap(_Y), (Expant = Mod:OutClause) ),
	Expant = tmp:TmpClause, % only for catching syntax errors/warnings.
	AssertThis = ad:slp_clause( Head, OutBody ),
	% write( user_error, asserting(AssertThis) ), nl( user_error ),
	assert( AssertThis ),
	bims:bims_bb_get( ad:all_preds, ADPreds ),
	( bims:ord_only_add_elem( ADPreds, Spec, NwADPreds ) ->
		bims:bims_bb_put( ad:all_preds,  NwADPreds )
		;
		true
	),
	Types = [ ns-non_stochastic, s-stochastic, d-distributional],
	( Stochastic == y -> 
		( ad_pt:pred_type(Spec,s) ->
			assert( Mod:s_label(Spec,Label) )
			;
			( bims:already_defined( Spec, s, Types, Cul2 ) -> 
				Cul1 = stochastic(Spec),
				pred_dfnd_err( Spec, Cul1, Cul2 )
				;
				assert( Mod:s_label(Spec,Label) ),
				assert( ad_pt:pred_type(Spec,s) )
			)
		)
		;
		( Stochastic == d ->
			( ad_pt:pred_type(Spec,d) ->
				assert( Mod:d_label(Spec,LEx,LVs) )
				;
				( bims:already_defined( Spec, d, Types, Cul2 ) ->
					Cul1 = distributional(Spec),
					pred_dfnd_err( Spec, Cul1, Cul2 )
					;
					assert( Mod:d_label(Spec,LEx,LVs) ),
					% write( user_error, distributional_type(Spec) ), nl( user_error ),
					assert( ad_pt:pred_type(Spec,d) )
				)
			)
			;
			( ad_pt:pred_type(Spec,ns) ->
				true
				;
				( bims:already_defined( Spec, ns, Types, Cul2 ) ->
					Cul1 = non_stochastic(Spec),
					pred_dfnd_err( Spec, Cul1, Cul2 )
					;
					assert( ad_pt:pred_type(Spec,ns) )
				)
			)
		)
	),
     % write( user_error, expant(Expant) ), nl( user_error ),
	true.
