:- ensure_loaded( library(sys_info) ).

select_compound_w_gunzip( [], [], [], [] ).
select_compound_w_gunzip( [H|T], Cmp, Oth, Gz ) :-
	( compound(H) ->
		Cmp = [H|TCmp], Oth = TOth, Gz = TGz 
		;
		Cmp = TCmp, 
		( atom_concat( Stem, '.gz', H ) ->
			Oth = [Stem|TOth],
			atom_concat( 'gunzip ', H, Gunzip ), 
			sys_info( Gunzip ),
			Gz = [Stem|TGz]
			;
			Oth = [H|TOth], Gz = TGz
		)
	),
	select_compound_w_gunzip( T, TCmp, TOth, TGz ).
