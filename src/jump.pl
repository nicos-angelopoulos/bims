:- ensure_loaded( library(on_random) ). 	% /6.
:- ensure_loaded( library(dbg) ). 			% dbg_this/1.
:- requires( call_error/3 ).
:- requires( if/2 ).

% assumes, user:llhood_w_diff/3.
% and user:display_model/4, if display_at bb_put is set.
%
jump( sc, _MODi, _MODst, _BLCntr, _I, _LLi, _LLst, _RecA, jump ) :-
	!.
jump( _, MODi, MODst, BLCntr, I, LLi, LLst, RecA, Jump ) :-
     % werr( i(I)-mi(MODi)-ms(MODst), warning ),
     % ( I =:= 4 -> trace; true ),
	call_error(lhood_canonical(MODst,MODi,LLst,LLi,Ratio),error,abort),
	report( ratio, ratio(I,Ratio) ),
	( bims_bb_get(gpr,true) -> 
		global_priors_ratio( MODst, MODi, GlobalPsR ),
		% write( user_error, gpr(GlobalPsR) ),
		Alpha is min( 1, (BLCntr * Ratio * GlobalPsR))
		;
		Alpha is min( 1, (BLCntr * Ratio))
	),
	RecA = a(BLCntr,Ratio,Jump),
	on_random( Alpha, jump, dont_jump, Rnd, _Whc, Jump ), 
	report( alpha, alpha(I,Alpha,Rnd,Jump,min(1,(BLCntr*Ratio))) ),
	%% Alpha is min( 1, (BCntr * ((1-LCi)/(1-LCst)) * Ratio)),
	% Alpha is min( 1, (Bp ** (Nst - Ni) * ((1-LCi)/(1-LCst)) * Ratio)),
	% 2005/01/22 on_random( Alpha, jump, dont_jump, _Rnd, _Whc, Jump ), 
	( bims_bb_get(progress,pts(Grs,F,Oth))->
		( F =< I ->
			write( user_error, Grs ),
			( Oth == [] ->
				nl( user_error ), bims_bb_delete(progress,_)
				; Oth = [NxPrg|TlPrg],
				  bims_bb_put(progress,pts(Grs,NxPrg,TlPrg))
			)
			; true
		)
		; true
	),
	Dmod = display_iteration_models(I,MODi,MODst,LLi,LLst,Alpha,Jump,PostCall,Out),
	if( display_iteration(I,PostCall,Out,Jump), Dmod ).

display_iteration( I, PostCall, Out, Jump ) :-
	pl( swi(_),
		( Key=display_at,
		  Err=error( existence_error(variable,Key), context(system:nb_getval/2, _)),
		  catch( user:nb_getval(Key,Old), Err, fail ),
		  Old = (I,At,PostCall,Out,Jump)
		)
		,
		bims_bb_get( display_at, (I,At,PostCall,Out,Jump) )
		% bb_get( user:display_at, (I,At,PostCall,Out,Jump) )
	),
	call( At ).
	% call( user:At ).

display_iteration_models( I, MODi, MODst, LLi, LLst, Alpha, Jump, PostCall, Out ) :-
	% write( Out, ith(MODi) ), nl( Out ),
	number_codes( I, ItCodes ),
	append( "model_", ItCodes, CurTitleCs ),
	append( "proposed_", ItCodes, PropTitleCs ),
	atom_codes( CurTitle, CurTitleCs ),
	atom_codes( PropTitle, PropTitleCs ),
	% user:display_model( MODi, [title(CurTitle)lhood(LLi),delete(false)] ),
	% exec(gv) and exec(xv) are  also valid options
	display_model( MODi, [title(CurTitle),lhood(LLi)] ),
	display_model( MODst, [title(PropTitle),lhood(LLst)] ),
	% user:display_model( MODi, [title(CurTitle),lhood(LLi)] ),
	% user:display_model( MODst, [title(PropTitle),lhood(LLst)] ),
	% write( Out, proposed(MODst) ), nl( Out ),
	write( Out, iteration(I) ), nl( Out ),
	write( Out, 'Alpha value was : ' ), write( Out, Alpha ),
	write( Out, '.     Decision was : ' ), write( Out, Jump ), nl(Out),
	call( PostCall ).
	% call( user:PostCall ).
