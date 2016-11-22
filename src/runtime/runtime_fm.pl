:- use_module( library(random) ).

%% :- bb_put( sid, 1 ).

% select_id( ClsArgs, InArgs, Lbls, Nth, FPId, FPTl, SPId, SelId, FPCont ) :-
% ClsArgs, the defined arguments of clauses.
% the Input Arguments.
% Lbls the labels 
% Nth  Numberic Id for first clause 
% FPId Id for first path
% select_id( CAs, IAs, Spec, LExs, LAs, N, FPI, FPT, SPI, SlI, FPCt ) :-

select_id_expr( Args, CArgs, Spec, LExs, LAs, Nth, H, T, Sel, Sid, Tail ) :-
	evaluate_lbl_expressions_for( Spec, CArgs, LAs, LExs, Lbls ),
	select_id( Args, CArgs, Lbls, Nth, H, T, Sel, Sid, Tail ).

select_id( Args, CArgs, Lbls, Nth, H, T, Sel, Sid, Tail ) :-
	( var(H) ->
		% maybe no Lbls = LExs, % by this stage % so
		add_unifying( Args, CArgs, Lbls, Nth, 0, 0, _Unifs, ULs, Sum  ),
		% first zero is a donot-exclude-any-id trick
		random( GotRnd ),
		Rnd is GotRnd * Sum,
		%% select_label( ULs, Rnd, 1, [], Sid, Sprb ),
		select_unified( Sum, ULs, Sid, Sprb ),
		length( ULs, Cardin ),
		cardin_to_path_point( Cardin, Sid, Sprb, Sel ),
		Tail = T
		;
		% ie this is the last pre-run choice point
		( T == [] ->
			H = PrvExcl/_ExcPrb, 
				% because last pre-run H cannot be Excl:ExtPrb
			user:bims_bb_get( exclude_lbl, x(PrvExcl,Excl) ),
			add_unifying( Args, CArgs, Lbls, Nth, Excl, 0, _Unifs, ULs, Sum  ),
				random( GotRnd ),
				Rnd is GotRnd * Sum,
				select_label( ULs, Rnd, 1, [], Sid, Sprb ),
				% select_unified( Sum, ULs, Sid, Sprb ),
			length( [H|ULs], Cardin ),
			cardin_to_path_point( Cardin, Sid, Sprb, Sel )
			% Tail is the left as a unconnected variable.
			;
			Tail = T,
			Sel = H,
			( H = Sid/Sprb -> true ; H = Sid:Sprb )
		)
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

% this will fail on select_unified, that had no unified, arguments.
/*
select_id( Args, A, Lbls, Nth, H, T, Sel, Sid, Tail ) :-
	%% write( lbls(Lbls) ), nl,
	% Unifs can be eleminated, keep it here, just in case,
	% we decide to hold it, as part of the path.
	( var(H) ->
		add_unifying( Args, A, Lbls, Nth, 0, 0, _Unifs, ULs, Sum  ),
		% first zero is a donot-exclude-any-id trick
		select_unified( Sum, ULs, Sid, Sprb ),
		length( ULs, Cardin ),
		Tail = T
		;
		( T == [] ->
			% write( run_h(H) ), nl,
			H = Excl/_ExcPrb,
			add_unifying( Args, A, Lbls, Nth, Excl, 0, _Unifs, ULs, Sum  ),
			% % Dec 15
			% random( Rnd ),
			% attach_ids_to_labels( ULs, 1, IdULs ),
			% write( select_label( IdULs, Rnd, 1, [], Sid, Sprb ) ), nl,
			% select_label( IdULs, Rnd, 1, [], Sid, Sprb ),
			% write( select_label( IdULs, Rnd, 1, [], Sid, Sprb ) ), nl,
			% end of Dec 15
			select_unified( Sum, ULs, Sid, Sprb ),
			% write( select_unified( Sum, ULs, Sid, Sprb ) ), nl,
			length( ULs, ULlgt ),
			( ULlgt =:= 1 -> Cardin is 0 ; Cardin is  2 )
			% Tail = _
			;
			Tail = T,
			( (H = Sid/Sprb;H = Sid:Sprb) ->
				Cardin is 2
				;
				Cardin is 1,
				Sid = H
			)
		)
	),
	( (Cardin>1,Sid=:=7,Sprb=:=1) -> trace ; true ),
	cardin_to_path_point( Cardin, Sid, Sprb, Sel ).
	*/

add_unifying( [], _U, _Empty, _Nth, _Excl, Sum, [], [], Sum ).
add_unifying( [H|T], U, [L|Ls], Nth, Excl, Acc, Unifs, ULs, Sum ) :-
	NxN is Nth + 1,
	( (Nth =\= Excl, \+ \+ H = U,L > 0) ->
		NxAcc is Acc + L,
		Unifs = [H|TUnifs],
		ULs = [L-Nth|TUls]
		;
		NxAcc is Acc,
		Unifs = TUnifs,
		ULs = TUls
	),
	add_unifying( T, U, Ls, NxN, Excl, NxAcc, TUnifs, TUls, Sum ).

select_unified( Sum, ULs, SelId, PrbSel ) :-
	random:random( R ),
	NormR is R * Sum, 
	select_from_unified_label( ULs, NormR, SelId, PrbSel ).

select_from_unified_label( [L-Id], _R, Id, L ) :- !.
select_from_unified_label( [L-Lid|T], R, Id, SeL ) :-
	( R =< L ->  % changed 2003-12-08, untested
	% ( R < L -> 
		Id = Lid, SeL = L, !
		;
		NxR is R - L,
		select_from_unified_label( T, NxR, Id, SeL )
	).

select_this_label( Id, Label, _NS, _NxTotMass, Id, Label ).
select_this_label( _AnId, _ALbl, FromThese, TotMass, Id, Label ) :-
	random( GotRnd ),
	Rnd is GotRnd * TotMass,
	select_label( FromThese, Rnd, TotMass, [], Id, Label ).

select_label( [HLbl-HId|T], Rnd, TotMass, Skp, Id, Label ) :-
	Rnd < HLbl,   % < because Rnd < 1 ?
	!,
	NxTotMass is TotMass - HLbl,
	lists:append( Skp, T, NonSel ),
	% Id = NthC...
	select_this_label( HId, HLbl, NonSel, NxTotMass, Id, Label ).
select_label( [HLbl-HId|T], Rnd, TotMass, Skp, Id, Label ) :-
	NxRnd is Rnd - HLbl,
	select_label( T, NxRnd, TotMass, [HLbl-HId|Skp], Id, Label ).

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
