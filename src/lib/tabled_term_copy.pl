tabled_copy_term( Term, STin, Add, CopyTerm, STout ) :-
	( var(Term) -> 
		var_to_table( STin, Add, Term, STout, CopyTerm )
		;
		( atomic(Term) -> 
			CopyTerm = Term,
			STin = STout
			;
			Term =.. [Name|Args],
			tabled_list_copy( Args, STin, Add, CopyArgs, STout ),
			CopyTerm =.. [Name|CopyArgs]
		)
	).

tabled_list_copy( [], ST, _Add, [], ST ).
tabled_list_copy( [H|T], STin, Add, [CopyH|CopyT], STout ) :-
	tabled_copy_term( H, STin, Add, CopyH, STnxt ),
	tabled_list_copy( T, STnxt, Add, CopyT, STout ).

var_to_table( [], Add, Var, Sou, CopyVar ) :-
	( Add == add ->
		Sou = [Var-CopyVar]
		;
		Sou = []
	).
var_to_table( [Lv-Rv|T], Add, Var, [Lv-Rv|Tout], CopyVar ) :-
	( Lv == Var ->
		CopyVar = Rv, Tout = T
		;
		var_to_table( T, Add, Var, Tout, CopyVar )
	).
