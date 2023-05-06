:- use_module( library(lists) ).		% needed for last_skips/2.

% fixme: 23.05.06: trying SLD resolution for finding all paths

select_id( Args, CArgs, LblsStr, Nth, H, Sid ) :-
	( var(H) ->
		( LblsStr = Spec/LExs/LAs -> 
			evaluate_lbl_expressions_for( Spec, CArgs, LAs, LExs, Lbls )
			;
			Lbls = LblsStr
		),
		add_unifying( Args, CArgs, Lbls, Nth, 0, _Unifs, ULs, _Sum  ),
		% random( GotRnd ),
		% Rnd is GotRnd * Sum,
		select_label( ULs, Sid, Sprb ),
		length( ULs, Cardin ),
		cardin_to_path_point( Cardin, Sid, Sprb, H )
		;
		( (H=Sid/_Hprb1;H=Sid:_Hprb2) -> true ;
			write( user_error, 'Unrecognised lead-point: ' ),
			write( user_error, H ), nl( user_error )
		),
		RelPos is Sid - Nth,
		unify_nth0( RelPos, Args, CArgs ),
		% fixme: is the following doing anything? Lbls are used nowhere...?!
		( LblsStr = Spec/LExs/LAs -> 
			% following call can be removed if, (a) pvars for all 
			% s-predicates required/are-present, and (b) following call is
			% adapted to use pvar definitions to do that on the fly.
			% Currently i think (a) is too expensive for the depth case in
			% carts and (b) will be quite expensive in all cases (we will
			% need to test for groundness- which i assume is more expensive
			% than testing for something being a variable.
			evaluate_lbl_expressions_for( Spec, CArgs, LAs, LExs, _Lbls1 )
			;
			_Lbls2 = LblsStr
		)
		% T = Tail
		% the above is experimental, use following if backtracking
		% over concrete parts of the path is required.
		% Currently we expect failures to be caught in kernel.
		% add_unifying( Args, CArgs, Lbls, Nth, Excl, 0, _Unifs, ULs, Sum  ),
		% select_this_label(...).
	).

% bb_get( pmember/2, ([Lst,_E],[Lgt-length(Lst,Lgt)]) )

% CArgs, are the called with arguments
evaluate_lbl_expressions_for( Spec, CArgs, LAs, LExs, Lbls ) :-
	% bb_get( Spec, Args, LblvarCallPairs ),
	( pvars:pvars( Spec, CArgs, LblvarCallPairs ) ->
		true ; true % uniinstantiated is fine
	),
	% maybe we can add, flag controlled, warning 
	ground_lbl_arguments( LAs, LblvarCallPairs ),
	evaluate_lbl_expressions( LExs, LAs, Lbls ).

ground_lbl_arguments( [], [] ).
ground_lbl_arguments( [H|T], [V-Call|Rest] ) :-
	( var(H) -> 
		H = V,
		% should add test for groundness of call,
		% but it will make it more expensive
		call( Call )
		;
		true
	),
	ground_lbl_arguments( T, Rest ).

evaluate_lbl_expressions( [], _Vals, [] ).
evaluate_lbl_expressions( [HExp-Vals|T], ValExps, [HLbl|TLbl] ) :-
	expressions_to_values( ValExps, Vals ),
	HLbl is HExp,
	evaluate_lbl_expressions( T, Vals, TLbl ).

expressions_to_values( [], [] ).
expressions_to_values( [H|T], [Hvl|Tvl] ) :-
	Hvl is H,
	expressions_to_values( T, Tvl ).

unify_nth0( N, [_H|T], U ) :-
	N > 0,
	!,
	NxN is N - 1,
	unify_nth0( NxN, T, U ).
unify_nth0( 0, [U|_T], U ).

add_unifying( [], _U, _Empty, _Nth, Sum, [], [], Sum ).
% add_unifying( [H|T], U, [L|Ls], Nth, Excl, Acc, Unifs, ULs, Sum ) :-
add_unifying( [H|T], U, [L|Ls], Nth, Acc, Unifs, ULs, Sum ) :-
	NxN is Nth + 1,
	% ( (Nth =\= Excl, \+ \+ H = U,L > 0) ->
	( (\+ \+ H = U,L > 0) ->
		NxAcc is Acc + L,
		Unifs = [H|TUnifs],
		ULs = [L-Nth|TUls]
		;
		NxAcc is Acc,
		Unifs = TUnifs,
		ULs = TUls
	),
	add_unifying( T, U, Ls, NxN, NxAcc, TUnifs, TUls, Sum ).

/*
select_unified( Sum, ULs, SelId, PrbSel ) :-
	random:random( R ),
	NormR is R * Sum, 
	select_from_unified_label( ULs, NormR, SelId, PrbSel ).

select_from_unified_label( [L-Id], _R, Id, L ) :- !.
select_from_unified_label( [L-Lid|T], R, Id, SeL ) :-
	R =< L,
	!,
	Id = Lid, SeL = L.
select_from_unified_label( [L-_Lid|T], R, Id, SeL ) :-
	NxR is R - L,
	select_from_unified_label( T, NxR, Id, SeL ).
	*/

% select_this_label( Id, Label, _NS, _NxTotMass, Id, Label ).
% select_this_label( _AnId, _ALbl, FromThese, TotMass, Id, Label ) :-
	% % random( GotRnd ),
	% %  Rnd is GotRnd * TotMass,
	% select_label( FromThese, TotMass, Id, Label ).

select_label( [HLbl-HId|_T], HId, HLbl ).
	% Rnd < HLbl,   % < because Rnd < 1 ?
	% !,
	% NxTotMass is TotMass - HLbl,
	% lists:append( Skp, T, NonSel ),
	% select_this_label( HId, HLbl, NonSel, NxTotMass, Id, Label ). %wb
	% HId = Id, Label = HLbl. %nb
select_label( [_|T], Id, Label ) :-
	% NxRnd is Rnd - HLbl,
	select_label( T, Id, Label ).

cardin_to_path_point( Cardin, Sid, Sprb, Sel ) :-
	( Cardin > 1 -> 
		Sel = Sid/Sprb
		;
		Sel = Sid:Sprb
	).
/* 
	This AVOIDS the creation of "pseudo"-backtracking points.
	If there was a single unifying head clause, then no point is added.
	It might be desirable in some cases to create such points, 
	for instance single clausal predicatess with 1 as their label. 
	This way, MCMC would be directed to also consider that point. 
	To implement such a strategy change Else part above, to: Sel = single(Sid)/Sprb).
	Each backtracking proposal should bb_put a token about whether
	these points can be backtracked to. Pick this token in 
	select_btrack_point/6 to stip single/1 or skip the point.
*/
