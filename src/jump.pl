% :- ensure_loaded( library(on_random) ). 	% /6.
:- lib(if/2).
:- lib(report/2).
:- lib(on_random/6). 	% /6.

% assumes, user:llhood_w_diff/3.
% and user:display_model/4, if display_at bb_put is set.
%
jump( sc, _MODi, _MODst, _BLCntr, _I, _LLi, _LLst, _RecA, jump ) :-
	!.
jump( _, MODi, MODst, BLCntr, I, LLi, LLst, RecA, Jump ) :-
	% Goal = lhood_canonical(MODst,MODi,LLst,LLi,Ratio),
	% Excp = error(evaluation_error(float_overflow),context(system:(is)/2,W)),
	% Actn = jump_swi_ex(W,Ratio,WFlg),
	% ( catch(Goal,Excp,Actn) -> )
	call_error( bims_lkl:lhood_canonical(MODst,MODi,LLst,LLi,Ratio), error, abort ),
	report( ratio, ratio(I,Ratio) ),
	( bims_bb_get(gpr,true) -> 
		global_priors_ratio( MODst, MODi, GlobalPsR ),
		Alpha is min( 1, (BLCntr * Ratio * GlobalPsR))
		;
		Alpha is min( 1, (BLCntr * Ratio))
	),
	RecA = a(BLCntr,Ratio,Jump),
	on_random( Alpha, jump, dont_jump, Rnd, _Whc, Jump ), 
	report( alpha, alpha(I,Alpha,Rnd,Jump,min(1,(BLCntr * Ratio))) ),
	%% Alpha is min( 1, (BCntr * ((1-LCi)/(1-LCst)) * Ratio)),
	% Alpha is min( 1, (Bp ** (Nst - Ni) * ((1-LCi)/(1-LCst)) * Ratio)),
	% 2005/01/22 on_random( Alpha, jump, dont_jump, _Rnd, _Whc, Jump ), 
	Disp = display_iteration(I,PostCall,Out,Jump),
	Dmod = display_iteration_models(I,MODi,MODst,LLi,LLst,Alpha,Jump,PostCall,Out),
	if( Disp, Dmod ).

display_iteration( I, PostCall, Out, Jump ) :-
	pl( swi(_),
		( Key=display_at,
		  Err=error( existence_error(variable,Key), context(system:nb_getval/2, _)),
		  catch( user:nb_getval(Key,Stored), Err, fail ),
		  copy_term( Stored, Old ),
		  Old = (I,At,PostCall,Out,Jump)
		)
		,
		bims_bb_get( user:display_at, (I,At,PostCall,Out,Jump) )
	),
	call( user:At ).

display_iteration_models( I, MODi, MODst, LLi, LLst, Alpha, Jump, PostCall, Out ) :-
	% write( Out, ith(MODi) ), nl( Out ),
	number_codes( I, ItCodes ),
	append( "model_", ItCodes, CurTitleCs ),
	append( "proposed_", ItCodes, PropTitleCs ),
	atom_codes( CurTitle, CurTitleCs ),
	atom_codes( PropTitle, PropTitleCs ),
	% user:display_model( MODi, [title(CurTitle)lhood(LLi),delete(false)] ),
	% exec(gv) and exec(xv) are  also valid options
	user:display_model( MODi, [title(CurTitle),lhood(LLi)] ),
	user:display_model( MODst, [title(PropTitle),lhood(LLst)] ),
	% write( Out, proposed(MODst) ), nl( Out ),
	write( Out, iteration(I) ), nl( Out ),
	write( Out, 'Alpha value was : ' ), write( Out, Alpha ),
	write( Out, '.     Decision was : ' ), write( Out, Jump ), nl(Out),
	call( user:PostCall ).

% jump_swi_ex(  _W, +inf, +inf ).
	% write( user_error, got_it(W) ), nl( user_error ).
