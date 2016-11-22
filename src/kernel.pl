:- ensure_loaded( library(lists) ).
:- ensure_loaded( construct_scall ).		% spec_constructs_scall/5,
                                             % pred_type_constructs_scall/5.
% :- pl( swi(_), ensure_loaded(jump_swi), ensure_loaded(jump) ).
:- pl( swi(_), requires(jump_swi/0), requires(jump/0) ).

:- requires( bp/0 ).  % assorted preds
:- requires( on_random/5 ).
:- requires( nst_length/3 ).
:- requires( rev_append/3 ).
:- requires( n_distinct_randoms_in/4 ).
:- requires( report/2 ).

% :- ensure_loaded( chains_metrics_dbg ).  % testing only

:- multifile( last_skips/2 ).
:- dynamic( last_skips/2 ).

:- multifile( mcmc_recovery/2 ).
:- dynamic( mcmc_recovery/2 ).


kernel( SGl, HotClss, I, Bid, P, ResO ) :-
	% SGl = Prd/Args,
	% copy_term( Args, FrArgs ),
	% ( bb_get(given_initial_model,MODiPrv)-> true; true ),
	SGl =.. [Prd|Args],
	append( Args, [MODiPrv], PlArgs ),
	length( PlArgs, Arity ),
	% Arity is PlLgth,
	spec_constructs_scall( Prd/Arity, Type, Path, PlArgs, Slp ),
	once( initial_bp_str(Bid,P,Str) ),
	( bims_bb_delete(initial_model_given,MODiPrv) -> true ; true ),
	bims_bb_get( msd, Msd ),
	( current_predicate(global_priors_ratio/3) ->
		bims_bb_put( gpr, true )
		;
		( bims_bb_delete( gpr, _ ) -> true ; true )
	),
     bims_bb_put( s_random_prod, [] ),
     bims_bb_put( s_random_avail, [] ),
     bims_bb_put( s_random_cons, [] ),
	initial_slp( Msd, Slp ),
	initial_hot_chain_models( HotClss, Bid, Prd/Arity, Type, Args, Msd, HotChns ),
	( current_predicate(bims_lkl:to_model/3) ->
		bims_lkl:to_model( MODiPrv, MODi, MODiKp ) 
		; 
		MODi = MODiPrv, MODiKp = MODi
	),
	path_to_btrack_pos( Bid, Path, Sop ), 
	            % fixme: above leaves backtrack point, is it appropriate here?
     report( b_pos, b_pos(1,Sop) ),
	( is_list(Sop) -> nst_length( Sop, 0, Ni ) ; Ni = nal ),
	% ( length( Sop, Ni ) -> true ; Ni = nal ), % nal = not_a_list
	% % reverse( Pos, Sop ),
	% (VorO == op -> BpStr = P ; BpStr = 1-1/2-P),
	% portray_clause( ResO, m(1,MODiKp) ),
	bims_lkl:model_llhood( MODi, LLi ),
	report( lkl_l, lkl_l(1,LLi,Ni) ),
	length( HotChns, LgthHotChns ), 
	HCLim is LgthHotChns + 2,
	!,
	transition( false, Path, Prd/Args/Type, Sop, MODi/MODiKp, 1, Ni, LLi, Bid/Str, HotChns/HCLim, 2, ResO, [iter(I)] ).

initial_slp( rm, Slp ) :-
	call( slp:Slp ),
	!.
initial_slp( fm, Slp ) :-
	repeat,
	call( slp:Slp ),
	!.

initial_hot_chain_models( [], _Bid, _Spec, _Type, _Args, _Msd, [] ).
initial_hot_chain_models( [H|T], Bid, Spec, Type, Args, Msd, HotMods ) :-
	append( Args, [HModPrv], PlArgs ),
	spec_constructs_scall( Spec, Type, Path, PlArgs, Slp ),
	initial_slp( Msd, Slp ),
	path_to_btrack_pos( Bid, Path, Sop ),
	( is_list(Sop) -> nst_length( Sop, 0, LgS) ; LgS = nal ),

	( current_predicate(bims_lkl:to_model/3) ->
		bims_lkl:to_model( HModPrv, HMod, HModKp )
		; 
		HMod = HModPrv, HModKp = HModPrv
	),
	model_llhood( HMod, LLh ),
	HotMods = [hc(HMod/HModKp,LLh,Path,Sop,LgS,H)|TMods],
	initial_hot_chain_models( T, Bid, Spec, Type, Args, Msd, TMods ).

transition( true, _InP, _SGl, _Sop, _MODi/MODiKp, Tms, _Ni, _LLi, _Str, _HotChns, _Iter, ResO, _Termin ) :-	!,
	portray_clause( ResO, m(Tms,MODiKp) ).
transition( false, InPath, SGl, Sop, MODi/MODiKp, Tms, Ni, LLi, Bid/Str, HotChns/HCLm, I, ResO, Termin ) :-
	% debug( bims, 'Iteration: ~d', I ),
	once( bp_str_to_bp(Bid,Str,Bp) ),
	once( backtrack_on_nxt( Bid, Bp, InPath, MODi/MODiKp, SGl, Sop, I, Ni, NwPath, PrvOSop, PrvNxNi, MODst/MODstKp, Nth, BCntr ) ),
	% write( user_error, model_star( I, MODst, MODstKp ) ), nl( user_error ),
	% since, now, all proposals are non-excluding, we can do away with next if
	( MODst == '__failure'  ->
		bims_bb_get( bk_failures, FsSoFar ),
		bims_bb_put( bk_failures, [I|FsSoFar] ),
		Fin = false, OutPath = InPath,
		OSop = Sop, NxMODi = MODi, NxMODiKp = MODiKp,
		NxNi = Ni, NxLL = LLi, NxTms = Tms,
		NxStr = Str, NxI = I
		;
		once( update_bp_str(Bid,Str,NxStr) ),
		NxI is I + 1,
		jump( Bid, MODi, MODst, BCntr, I, LLi, LLst, RecA, Jump ),
		bims_report_progress( I ),
		report( m_star, m_star(I,MODstKp) ),
		report( l_star, l_star(I,LLst,Nth,RecA) ),
		( Jump==jump ->
			% backtrack_on_nxt/n might have instantiate them.
			% this is the case for Bid=uc, since the length is needed
			% for computing BCntr
			( MODiKp == MODstKp -> NxTms is Tms + 1 % fixme: we assume this is desirable, ie when jumping back and accepting 
			                                        % the same or isomorphic model
							; portray_clause( ResO, m(Tms,MODiKp) ),
							  NxTms is 1
			),
			( var(PrvOSop) ->
				path_to_btrack_pos( Bid, NwPath, CldOSop ),
				% % reverse( NwPos, OSop ),
				( is_list(CldOSop) ->
					% only do that for Bids that need it.
					% nst_length( OSop, 0, NxNi ) % length( OSop, NxNi ) 
					length( CldOSop, CldNxNi )
					; 
					CldNxNi = nal 
				)
				;
				CldOSop = PrvOSop,
				CldNxNi = PrvNxNi
			),
			CldOutPath = NwPath, CldNxLL = LLst, CldNxMODi = MODst,
			CldNxMODiKp = MODstKp
			; 
			NxTms is Tms + 1,
			CldOutPath = InPath, CldNxNi = Ni, CldNxLL = LLi,
			CldNxMODi = MODi, CldNxMODiKp = MODiKp, CldOSop = Sop
		),
          report( b_pos, b_pos(I,Sop) ),
		( termination( Termin, Jump, I ) ->
			% Model = Jump-OutPath
			bims_bb_put( last_model, c(NxMODi) ),
			Fin = true
			;
			Fin = false
		)
	),
	( HotChns == [] ->
		NxHotChns = [],
		OutPath = CldOutPath, NxNi = CldNxNi, NxLL = CldNxLL,
		NxMODi = CldNxMODi, NxMODiKp = CldNxMODiKp, OSop = CldOSop
		;
		Nil = hc(CldNxMODi/CldNxMODiKp,CldNxLL,CldOutPath,CldOSop,CldNxNi,1),
		power_backtrack( HotChns, Bid, Bp, SGl, PrvNxHotChns ),
		n_distinct_randoms_in( 2, 1, HCLm, [Rnd1,Rnd2] ),
		power_chain_swap( Rnd1, Rnd2, [Nil|PrvNxHotChns], HotJmp, [Fst|NxHotChns] ),
		( (Rnd1 =:= 1, HotJmp == jump) -> 
			Fst = hc(NxMODi/NxMODiKp,NxLL,OutPath,OSop,NxNi,_)
			;
			OutPath = CldOutPath, NxNi = CldNxNi, NxLL = CldNxLL,
			NxMODi = CldNxMODi , OSop = CldOSop,
			% Temporarily only:
			NxMODiKp = CldNxMODiKp
		)
	),
	!,
	% % chains_metrics_dbg( NxMODi, NxHotChains ), % testing only
	% portray_clause( ResO, m(NxI,NxMODiKp) ),
	report( lkl_l, lkl_l(NxI,NxLL,NxNi) ),
	transition( Fin, OutPath, SGl, OSop, NxMODi/NxMODiKp, NxTms, NxNi, NxLL, Bid/NxStr, NxHotChns/HCLm, NxI, ResO, Termin ).

backtrack_on_nxt( sc, _Bp, _InPath, MODi/MODiKp, SGl, _Sop, _I, _LgS, [], _NwSop, _NwLgS, MODst/MODstKp, _Nth, _BLCntr ) :-
	!,
	findall( VId, scm_gen_each_vid(MODi,VId), VIds ),
	scm_iterate( VIds, SGl, MODi, MODiKp, MODst, MODstKp ).
backtrack_on_nxt( uc, Bp, InPath, _MODi, SGl, Sop, It, LgS, NwPath, NwSop, NwLgS, MODstWK, Nth, BLCntr ) :-
	!,
	backtrack_on_nxt_1( uc, Bp, InPath, _, SGl, Sop, It, LgS, NwPath, MODstWK, Nth, _Sel, _BCntr ),
	% % % split_on_nth( NwPath, Nth, _Within, _FNth, uc/NwCntr, _Left ),
	path_to_btrack_pos( uc, NwPath, NwSop ),
	nst_length( NwSop, 0, NwLgSPrv ),
	NwLgS is max( NwLgSPrv, 1 ),
	BLCntr is LgS / NwLgS.
backtrack_on_nxt( Bid, Bp, InPath, _MODi, SGl, Sop, It, LgS, NwPath, _NwSop, _NwLgS, MODstWK, Nth, BLCntr ) :-
	backtrack_on_nxt_1( Bid, Bp, InPath, _MODiDsh, SGl, Sop, It, LgS, NwPath, MODstWK, Nth, _Sel, BLCntr ).
	% % % backtrack_on_nxt_1( Bid, Bp, InPath, _MODi, SGl, Sop, LgS, NwPath, MODstWK, Nth, _Sel, BLCntr ).

% backtrack_on_nxt_1( Bid, Bp, InPath, _MODi, Prd/Args/Type, Sop, LgS, NewPath, MODst, Nth, BLCntr ) :-
% Nolabels contributions so, BCntr
% backtrack_on_nxt_1( Bid, Bp, InPath, MODi/MODiKp, Prd/Args/Type, Sop, LgS, NewPath, MODst/MODstKp, Nth, Sel, BCntr ) :-
backtrack_on_nxt_1( Bid, Bp, InPath, MODi/MODiKp, Prd/Args/Type, Sop, It, LgS, NewPath, MODst/MODstKp, Nth, Sel, BCntr ) :-
	( select_btrack_point( Bid, Sop, LgS, Bp, Nth, Sel, BCntr ) ->
		report( path, path(It,InPath) ),
		( Sel == +inf ->
			NewPath = InPath,
			MODst   = MODi, MODstKp = MODiKp,
			Nth	   = +inf, % i think this is only used for printing
			BCntr  is 1
			;
			( Nth =:= 0 -> 
				true    % let Left be an uninstantiated variable
				;
				split_on_nth( InPath, Nth, _Within, _FNth, Bid/BCntr, Left )

			),
			% copy_term( Args, FrArgs ),
               % write( user_error, inpath(InPath) ), nl( user_error ),
			append( Args, [MODstPrv], PlArgs ),
			pred_type_constructs_scall( Type, Prd, Left/[], PlArgs, Slp ),
			( call(slp:Slp) ->
				( current_predicate(bims_lkl:to_model/3) ->
					bims_lkl:to_model( MODstPrv, MODst, MODstKp )
					; 
					MODst = MODstPrv, MODstKp = MODstPrv
				),
				NewPath = Left
				;
				( bims_bb_get(msd,fm) ->
				% (\+ bb_get(exclude_lbd,false);bb_get(msd,fm)) ->
					MODst = '__failure'
					;
					bims_bb_get(msd,Msd),
					werr( [['Unable to generate model under model structure definition: \'',Msd,'\''],[slp_call(Slp)]] ),
					abort
				)
			)
		)
		;
		werr( [['Unable to backtrack. '],[sop(Sop),bid(Bid/Bp)]] ),
		abort
	).

path_to_btrack_pos( bu, Path, Pos ) :-
	!,
	rec_cps( Path, 1, 0, 0, [], 0, 0, [], _N, Pos ).
path_to_btrack_pos( cd, Path, Pos ) :-
	!,
	% HERE a
	path_to_nst_btrack_points( Path, 1, 0, _LstN, Pos ).
path_to_btrack_pos( Bid, Path, Pos ) :-
	path_to_btrack_points( Path, 1, 0, _Cont, PrvPos/[] ),
	% path_to_btrack_points( Path, 1, 0, ClnPath, _Cont, PrvPos/[] ),
	% HERE
	% path_to_btrack_points( Path, 1, PrvPos ),
	once( bid_points_to_pos(Bid,PrvPos,Pos) ).

/* need to uncomment its call too
% path_to_btrack_points( [], _Ind, [] ).
path_to_btrack_points( [H|T], Ind, Pos ) :-
	( H=_Id/Prb ->
		Pos = [Ind-Prb|TPos]
		;
		Pos = TPos
	),
	NxInd is Ind + 1,
	path_to_btrack_points( T, NxInd, TPos ).
*/

path_to_nst_btrack_points( [], N, _Last, N, [] ).
path_to_nst_btrack_points( [H|T], N, Last, LstN, Pos ) :-
	( is_list(H) -> 
		Pos = [HPos|TPos],
		path_to_nst_btrack_points( H, N, 0, NxN, HPos ),
		NxLst = 0
		;
		NxN is N + 1,
		( H=Id/Prb ->
			NxLst = Id,
			( last_skips( Last, Id ) ->
				TPos = Pos
				;
				Pos = [N-Prb|TPos]
			)
			;
			Pos = TPos,
			NxLst = 0
		)
	),
	path_to_nst_btrack_points( T, NxN, NxLst, LstN, TPos ).

path_to_btrack_points( [], Cont, _Last, Cont, T/T ).
% path_to_btrack_points( [H|T], Ind, Last, ClnPath, Cont, Pos/TPos ) :-
path_to_btrack_points( [H|T], Ind, Last, Cont, Pos/TPos ) :-
	( is_list(H) ->
		path_to_btrack_points( H , Ind, 0, NxInd, Pos/MdPos ),
		% ClnPath = [ClnH|ClnT],
		Id = 0
		;
		( H=Id/Prb ->
			%( ((Last==8;),(Id==8;Id==7)) ->
			( last_skips( Last, Id ) ->
				Pos = MdPos
				;
				Pos = [Ind-Prb|MdPos]
			),
			% ClnPath = [H|ClnT],
			NxInd is Ind + 1
			;
			Pos = MdPos,
			( H = bp(_), 			% HERE, this is not valid any longer
				NxInd is Ind,
				% ClnPath = ClnT,
				Id = Last
				;
				NxInd is Ind + 1,
				% ClnPath = [H|ClnT],
				Id = 0
			)
		)
	),
	path_to_btrack_points( T, NxInd, Id, Cont, MdPos/TPos ).

% % last_skips( 2, Id ) :-
	% !, 
	% (Id =:= 1; Id =:= 2 ).
/*
last_skips( 8, Id ) :-
	!, 
	(Id =:= 7; Id =:= 8 ).
last_skips( 6, Id ) :-
	( Id =:= 5; Id =:= 6 ).
	*/

rec_cps( [], N, _Lt, CntThese, InThese, CntAlts, CntAltPts, InAlts, N, Poss ) :-
	reverse( InThese, These ),
	reverse( InAlts, Alts ),
	Poss = cps( CntThese, These, CntAlts, CntAltPts, Alts ).
rec_cps( [H|T], N, Lt, CntTh, AccThese, CntA, CntAPs, AccAlts, FinN, Poss ) :-
	( is_list(H) -> 
		rec_cps( H, N, 0, 0, [], 0, 0, [], NxN, HCPs ),
		HCPs = cps(CntH,_,CntAltH,_,_),
		( (CntH=:=0,CntAltH=:=0) ->
			% zap it out
			NxCntTh is CntTh, NxAccThese =  AccThese,
			NxCntA  is CntA, NxCntAPs is CntAPs, NxAccAlts = AccAlts
			;
			NxCntTh is CntTh, NxAccThese =  AccThese,
			NxCntA  is CntA + 1, NxCntAPs is CntAPs + CntH + CntAltH,
			NxAccAlts = [HCPs|AccAlts]
		),
		NxLt = 0
		;
		NxN is N + 1,
		( H = Hid/HPrb	->			% a backtrack point
			NxLt = Hid,
			( last_skips(Lt,Hid) ->
				NxCntTh is CntTh, NxAccThese =  AccThese,
				NxCntA  is CntA, NxCntAPs is CntAPs, NxAccAlts = AccAlts
				;
				NxCntTh is CntTh + 1, NxAccThese = [N-HPrb|AccThese],
				NxCntA is CntA, NxCntAPs is CntAPs, NxAccAlts = AccAlts
			)
			;
			NxCntTh is CntTh, NxAccThese =  AccThese,
			NxCntA  is CntA, NxCntAPs is CntAPs, NxAccAlts = AccAlts,
			NxLt = H
		)
	),
	rec_cps( T, NxN, NxLt, NxCntTh, NxAccThese, NxCntA, NxCntAPs, NxAccAlts, FinN, Poss ).

select_btrack_point( bu, Points, _NoBPoints, K, Nth, _Sel, Cntr ) :-
	!,
	Cntr is 1,
	bu_select_bp( Points, K, Nth-_Prb ).
	
select_btrack_point( cd, Points, NoBPoints, P, Nth, _Sel, Cntr ) :-
	!,
	select_ind_btrack_point( P, Points, NoBPoints, Nth ),
	Cntr = 1. % everything cancels out in evaluating jump-alpha 
select_btrack_point( uc, Points, NoBPoints, _P, NthPoint, Nth, _Cntr ) :-
	% Nth = Sel(ected)
	!,
	Lim is NoBPoints + 1,
	random( 1, Lim, Nth ),
	nth( Nth, Points, NthPoint-_NPrb ).
	% 2005/01/21: we instantiate this later.
	% Cntr = 1. % everything cancels out in evaluating jump-alpha 
select_btrack_point( Bid, Points, _Lg, P, NthPoint, _Nth, Cntr ) :-
	select_dep_btrack_point( Points, Bid, P, 1, NthPoint, Cntr ).

% select_ind_btrack_point( _Curr, _NoBPoints, Nth ) :-
	% Nth = 7.
select_ind_btrack_point( Curr, Points, NoBPoints, NthPoint ) :-
	( NoBPoints < Curr -> Nth = NoBPoints ; Nth = Curr ),
	( Nth =:= 0 -> NthPoint = 0 
		; 
		% HERE b
		breadth_first_nth( Points, Curr, [], [], NthPoint )
		% nth( Nth, Points, NthPoint-_NPprb)
	).

select_dep_btrack_point( [H-HPrb|T], Bid, P, Acc, Nth, Cntr ) :-
	( T == [] ->
		% dbg( 30, controlled_sample_from_last(H) ),
		Nth = H, Cntr = Acc   % check that this is correct for all BPids
		;
		( continue_backtracking(Bid,P,HPrb,HCntr) ->
			NxAcc is Acc * HCntr,
			select_dep_btrack_point( T, Bid, P, NxAcc, Nth, Cntr )
			;
			((Nth = H, Cntr = Acc) ; 
				  select_dep_btrack_point( T, Bid, P, Acc, Nth, Cntr ) ) 
				  % by using Acc we probably say that this was not a real choice point
				  % which seems a reasonable enough statement
		)
	).

breadth_first_nth( [], N, AccFlat, AccRec, NthK ) :-
	( N == []	-> 	% get out of recursion
		true
		;
		( AccFlat == [] ->
			( AccRec == [] ->
				NthK = +inf
				;
				reverse( AccRec, Rec ),
				breadth_first_nth( Rec, N, [], [], NthK )
			)
			;
			reverse( AccFlat, Flat ), 
			reverse( AccRec, Rec ),
			append( Flat, Rec, FlatNRec ),
			breadth_first_nth( FlatNRec, N, [], [], NthK )
		)
	).
breadth_first_nth( [H|T], N, AccFlat, AccRec, NthK ) :-
	( is_list(H) ->
		flatten_one_level( H, HFlat, HRec ), 
		rev_append( HFlat, AccFlat, NxFlat ),
		rev_append( HRec, AccRec, NxRec ),
		NxN is N, NxT = T
		; 
		% we assume any non-list element is a pair structure
		( N =:= 1 ->
			H = NthK-_Prb,
			NxN = [],
			NxT = []
			;
			NxT = T,
			NxN is N - 1
		),
		NxFlat = AccFlat,
		NxRec  = AccRec
	),
	breadth_first_nth( NxT, NxN, NxFlat, NxRec, NthK ).

flatten_one_level( [], [], [] ).
flatten_one_level( [H|T], Flat, Nst ) :-
	( is_list(H) ->
		TFlat = Flat,
		Nst = [H|TNst]
		;
		Flat = [H|TFlat],
		TNst = Nst
	),
	flatten_one_level( T, TFlat, TNst ).

% nth_replace_column( Nth, InPath, LblCst, NewPath ) :-
nth_replace_column( Nth, [H|T], LblCst, NewPath ) :-
	( Nth =< 1 -> 
		( H = Sid:LblCst -> 
			NewPath = [Sid|T]
			;
			H = _Sid/_LblCst,
			NewPath = [H|T]
		)
		;
		% Sid=a, % just trick SWI's erroneous 
		       % singleton variable in branch warning
		NxN is Nth - 1,
		NewPath = [H|NewT],
		nth_replace_column( NxN, T, LblCst, NewT )
	).

split_on_nth( [], Nth, false, Nth, _BStr, [] ) :- !.
split_on_nth( [H|T], Nth, OutWithin, FNth, BStr, Left ) :-
	Nth > 1,
	!,
	( is_list(H) ->
		split_on_nth( H, Nth, NestWithin, NxNth, BStr, HLeft ),
		( NestWithin == true -> 
			Rem = [],
			Left = [HLeft|T],
			OutWithin = true
			; 
			Rem = T,
			Left = [H|TLeft],
			RemWithin = OutWithin
		)
		;
		Rem = T,
		Left = [H|TLeft],
		NxNth is Nth - 1,
		RemWithin = OutWithin
	),
	split_on_nth( Rem, NxNth, RemWithin, FNth, BStr, TLeft ).
split_on_nth( [H|T], Nth, true, Nth, BStr, Left ) :-
	Nth =:= 1,
	( is_list(H) -> 
		split_on_nth( H, 1, true, _, BStr, RecLeft ),
		Left = [RecLeft|T]
		; 
		( BStr = uc/BCntr -> 
			% nst_length( T, 0, BCntr )
			% path_to_btrack_pos( uc, T, TrailCPs ),
			( H=Lst/_Lprb -> true 		% this should always be the case
						; Lst = 0 ), 	% remove if after testing.
			path_to_btrack_points( T, 1, Lst, _Cont, TrailCPs/[] ),
			length( TrailCPs, BCntr )
			;
			true
		)
	).
% Let Left uninstantiated.
/*  bp/1 version
split_on_nth( [_H|_Right], Nth, true, Nth, Left ) :-
	( H = [F|_RcT] ->
		Left = [[bp(F)|_]|Right]
		;
		Left = [bp(F)|_]
	).
	*/

split_is_or_not_in_list( [], Nth, Nth, false, [] ).
split_is_or_not_in_list( [H|T], Nth, OutNth, Within, Pfx ) :-
	Nth > 1,
	!,
	NxNth is Nth - 1,
	Pfx = [H|TPfx],
	split_is_or_not_in_list( T, NxNth, OutNth, Within, TPfx ).
split_is_or_not_in_list( [H|_T], Nth, Nth, true, [H|_] ).
	% Value of OutNth doesnt matter here anyway.
	% The unanymous variable in last argument is in line with one path 
	% proposed trailing. It should work for the current two path trailing.

/* 2005/01/14, this is fine for non nested paths.
split_on_nth( Nth, [H|T], [H|Left], Right ) :-
	Nth > 1,
	!,
	NxNth is Nth - 1,
	split_on_nth( NxNth, T, Left, Right ).
split_on_nth( _Nth, [H|Right], [H], Right ).
*/

bu_select_bp( done, _K, _Sel ) :- !.
bu_select_bp( cps(CntTh,These,CntAlts,CntAltPts,Alts), K, Sel ) :-
	random( Rnd ),
	(CntTh =:= 0 -> 
		Nth0Alt is integer( CntAlts * Rnd ),
		% random( 0, CntAlts, Nth0Alt ),
		nth0( Nth0Alt, Alts, RecCps )
		;
		Here is CntTh / (CntTh + (CntAltPts * K)),
		( Rnd < Here -> 
			% Nth0 is integer( Rnd * Here * CntTh ),
			random( 0, CntTh, Nth0 ),
			nth0( Nth0, These, Sel ),
			RecCps = done
			;
			% Nth0 is integer( (1 - Here) * CntAlts * Rnd ),
			random( 0, CntAlts, Nth0 ),
			nth0( Nth0, Alts, RecCps )
		)
	),
	bu_select_bp( RecCps, K, Sel ).

/*
split_on_nth_or_earlier( Nth, [H|T], [H|Left], Right ) :-
	Nth >= 1,
	NxNth is Nth - 1,
	split_on_nth_or_earlier( NxNth, T, Left, Right ).
split_on_nth_or_earlier( _Nth, Right, [], Right ).
*/

termination( Termin, _Jump, I ) :-
	( memberchk( iter(IterLim), Termin ) ->
		I >= IterLim
		;
		report( iter, iter(I) ),
		fail
	).

%
% scm_iterate( +Ids, +SGl, +MODr, +MODrKp, -MODst, -MODstKp ) :-
% for model modifier ids Ids, goal structure SGl recursively changed
% model MODr/MODrKp final recursion's MODr/MODrKp are MODst/MODstKp.
% Each modifier pinpoints a part of input MODr which will be modified.
% Modified model is stochastically compared to input model, resulting
% to one of the two being the next iteration's (input) model.
%
scm_iterate( [], _, MODst, MODstKp, MODst, MODstKp ).
scm_iterate( [Hstr|T], Prd/Args/Type, MODr, MODrKp, MODst, MODstKp ) :-
	( Hstr = H-_LstA -> true; H = Hstr ),
	scm_gen_vstructure( MODr, H, Vstr, ExV, RepV ),
	% this is superfluous in our experiments to-date
	copy_term( Args, PrvFrArgs ),
	( Hstr = _Hag-LastArg -> 
		% last( FrArgs, LastArg )
		replace_last( PrvFrArgs, LastArg, FrArgs )
		;
		PrvFrArgs = FrArgs
	),
	append( FrArgs, [Vstr], PlArgs ),
	pred_type_constructs_scall( Type, Prd, _Left/_Sin, PlArgs, Slp ),
	call( slp:Slp ),
	( current_predicate(bims_lkl:to_model/3) ->
		bims_lkl:to_model( Vstr, NxMODrPrp, NxMODrKpPrp )
		; 
		NxMODrPrp = Vstr, NxMODrKpPrp = Vstr
	),
	scm_rel_likelihood( RepV, ExV, RelL ),
	Alpha is min(1,RelL),
	on_random( Alpha, t, t, _Rnd, Whc, _ChV ),
	% bef 2007/01/03, was: on_random( Alpha, RepV, ExV, _Rnd, Whc, _ChV ),
	( Whc == first -> NxMODr = NxMODrPrp, NxMODrKp = NxMODrKpPrp
				 ; NxMODr = MODr, NxMODrKp = MODrKp ),
	scm_iterate( T, Prd/Args/Type, NxMODr, NxMODrKp, MODst, MODstKp ).

replace_last( [_], Last, [Last] ) :- !.
replace_last( [H|T], Last, [H|R] ) :-
	replace_last( T, Last, R ).

bims_report_progress( I ) :-
	( bims_bb_get(progress,pts(Grs,F,Oth)) ->
		( F =< I ->
			write( user_error, Grs ),
			( Oth == [] ->
				nl( user_error ), bims_bb_delete(progress,_)
				; Oth = [NxPrg|TlPrg],
				  bims_bb_put(progress,pts(Grs,NxPrg,TlPrg))
			),
			flush_output( user_error )
			; true
		)
		; true
	).
