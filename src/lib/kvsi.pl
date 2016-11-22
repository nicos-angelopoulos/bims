% kvsi, key-value pairs sorted on integer key.
% should also go to lib 
kvsi_till_left_incl( [Hk-Hv|T], MarK, Left ) :-
	( MarK =:= Hk -> 
		Left = [Hk-Hv]
		;
		Left = [Hk-Hv|Teft],
		kvsi_till_left_incl( T, MarK, Teft )
	).

% key_and_right
kvsi_val_w_right_rem( [], _Mark, _Mv, _Right ) :-
	write( error_of_emptyness ), abort.
kvsi_val_w_right_rem( [Hk-Hv|T], Mark, Mv, Right ) :-
	( Hk < Mark -> 
		kvsi_val_w_right_rem( T, Mark, Mv, Right )
		;
		( Hk =:= Mark -> 
			Mv = Hv,
			Right = T
			;
			write( error_of_pairs ), abort
		)
	).


% lib
kvsi_merge_w_add( [], Slist, Slist ) :- !.
kvsi_merge_w_add( List, [], List ).
kvsi_merge_w_add( [Hk-Hv|T], [Sk-Sv|Ts], [Hm|Tm] ) :-
        ( Hk < Sk -> 
                Hm = Hk-Hv,
                kvsi_merge_w_add( T, [Sk-Sv|Ts], Tm )
                ;
                ( Hk =:= Sk ->
                        Nv is Hv + Sv,
                        Hm = Hk-Nv,
                        kvsi_merge_w_add( T, Ts, Tm )
                        ;
                        Hm = Sk-Sv,
                        kvsi_merge_w_add( [Hk-Hv|T], Ts, Tm )
                )
        ).

kvis_val_left_incl( [], Mark, _Mval, _Left ) :-
	write( couldnt_find_key_at_end(Mark) ), abort.
kvis_val_left_incl( [Hk-Hv|T], Mark, Mval, Left ) :-
	( Hk < Mark -> 
		Left = [Hv|Teft], 
		kvis_val_left_incl( T, Mark, Mval, Teft )
		;
		( Hk =:= Mark -> 
			Mval = Hv, Left = [Hv]
			;
			write( couldnt_find_key(Mark) ), abort
		)
	).

kv_replace_first_vals_w_rem( [], Tps, [], Tps ).
kv_replace_first_vals_w_rem( [H|T], [Hk-_Hv|Tps], [Hk-H|Tnwps], Rest ) :-
	kv_replace_first_vals_w_rem( T, Tps, Tnwps, Rest ).

kv_val_times_n( [], _Mult, [] ).
kv_val_times_n( [Hk-Hv|T], Mult, [Hk-Mv|Tm] ) :-
	Mv is Hv * Mult,
	kv_val_times_n( T, Mult, Tm ).

kv_sum( [_Hk-Hv|T], Sum ) :-
	kv_sum( T, Hv, Sum ).

kv_sum( [], Sum, Sum ).
kv_sum( [_Hk-Hv|T], Acc, Sum ) :-
	NxAcc is Acc + Hv,
	kv_sum( T, NxAcc, Sum ).

kv_mult_all( [], _Mult, [] ).
kv_mult_all( [Hk-Hv|T], Mult, [Hk-Hm|Tm] ) :-
	Hm is Hv * Mult,
	kv_mult_all( T, Mult, Tm ).

kv_div_all( [], _Div, [] ).
kv_div_all( [Hk-Hv|T], Div, [Hk-Hm|Tm] ) :-
	Hm is Hv / Div,
	kv_div_all( T, Div, Tm ).
