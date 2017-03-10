% all preds that make alternative backtrack strategies/probabilities possible.
% BId = backtrack ID
% The only issue that is not present here, is choosing the actual
% bactracking point from the available ones. See select_btrack_point/6,
% kernel.pl
%
:- ensure_loaded( library(lists) ).		% reverse/2.
:- ensure_loaded( library(random) ).		%/1.

bp. % loads the file

initial_bp_str( bp, Bp, Bp ).
initial_bp_str( cp, Bp, 1-1/2-Bp ).
initial_bp_str( pr, _Bp, pr ).
initial_bp_str( cd, Bp, BpStr ) :-
	( number(Bp) -> 
		BpStr = cd(1,1,Bp)  % BpStr = cd(1,1,Bp) 
		;
		( Bp = Min-Max -> 
			BpStr = cd(Min,Min,Max)
			; 
			write( user_error, unrecognised_cd_parameter(Bp)),
			nl( user_error ),
			abort
		)
	).
initial_bp_str( uc, _Bp, uc ).
% initial_bp_str( rc, _Bp, rc ). % emulate uc without ratio adjustment.
initial_bp_str( sc, _Bp, sc ).
initial_bp_str( bu, K, K ).
initial_bp_str( Un, _, _ ) :-
	werr( [['Unknown backtracking identifier: ',Un]] ),
	abort.

bp_str_to_bp( bp, Bp, Bp ).          % original, pbc backtracking
bp_str_to_bp( cp, _Pth-Bp-_Pl, Bp ). % cyclic pbc
bp_str_to_bp( pr, _, pr ). 		  % proportional 
bp_str_to_bp( cd, cd(C,_Mn,_Mx), C ).% cyclic deterministic
                                     % does cd(0-0) emulate indep. sampling ?
bp_str_to_bp( uc, _, uc ). 		  % uniform choice
% bp_str_to_bp( rc, _, rc ). 		  % test; a version of faulty uc.2005ba19
bp_str_to_bp( sc, _, sc ). 		  % single component
bp_str_to_bp( bu, K, K ). 		  % balanced uniform, 2005/01/19 
							  % K = 1, each level's choices take prb
							  % space proportional to their number.
							  % K < 1 favours higher levels.

update_bp_str( bp, Str, Str ).
update_bp_str( cp, Pth-_Bp-Pl, NxPth-NxBp-Pl ) :-
	( Pth =:= Pl ->
		NxPth = 1,
		NxBp  = 1/2
		;
		NxPth is Pth + 1,
		Denom is 2 ** NxPth,
		NxBp is (Denom - 1) / Denom
	).
update_bp_str( pr, Str, Str ).
update_bp_str( cd, InStr, OutStr ) :-
	InStr = cd(Curr,Min,Max),
	( Curr >= Max -> 
		OutStr = cd(Min,Min,Max)
		;
		Nxt is Curr + 1,
		OutStr = cd(Nxt,Min,Max)
	).
update_bp_str( uc, Str, Str ).
% update_bp_str( rc, Str, Str ).
update_bp_str( sc, Str, Str ).
update_bp_str( bu, Str, Str ).

% dependent only bids
continue_backtracking( bp, Bp, _HPrb, Bp ) :-
	random( Rnd ), Rnd =< Bp.
continue_backtracking( cp, Bp, _HPrb, Bp ) :-
	random( Rnd ), Rnd =< Bp.
continue_backtracking( pr, _Bp, HPrb, HPrb ) :-
	random( Rnd ), Rnd =< HPrb.

% bid_points_to_pos( bu, ... )   doesnt need that. bu is supported in the 
% kernel
bid_points_to_pos( cd, Pos, Pos ) :- !.
bid_points_to_pos( uc, Pos, Pos ) :- !.
% bid_points_to_pos( rc, Pos, Pos ) :- !.
bid_points_to_pos( _, Pos, Sop ) :-
	reverse( Pos, Sop ).

% 2005/01/15. All labels are internally taken to exclude label.
/*
bid_exclude_lbl( cd ) :-
	!,
	bb_put( exclude_lbl, false ).
bid_exclude_lbl( uc ) :-
	!,
	bb_put( exclude_lbl, false ).
% bid_exclude_lbl( cd ) :-
	% !,
	% bb_put( exclude_lbl, x(_,-1) ).
% bid_exclude_lbl( uc ) :-
	% !,
	% bb_put( exclude_lbl, x(_,-1) ).
% bid_exclude_lbl( _ ) :-
	% bb_put( exclude_lbl, x(Excl,Excl) ).

backtrack_and_label_contribution( cd, BCntr, _LblCi, _LblCst, BCntr ) :-
	!.
backtrack_and_label_contribution( uc, BCntr, _LblCi, _LblCst, BCntr ) :-
	!.
backtrack_and_label_contribution( _, BCntr, LCi, LCst, BandLCntr ) :-
	BandLCntr is BCntr * ((1-LCi)/(1-LCst)).
*/
