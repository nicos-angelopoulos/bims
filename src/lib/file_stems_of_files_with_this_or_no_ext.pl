file_stems_of_files_with_this_or_no_ext( [], _Ext, [] ).
file_stems_of_files_with_this_or_no_ext( [H|T], Ext, Stems ) :-
	( atom_concat(Stem,Ext,H) ->
		Stems = [Stem|TStems]
		;
		atom_codes( H, HCs ),
		( append( _StemCs, [0'.|_], HCs ) ->
			TStems = Stems
			;
			Stems = [H|TStems]
		)
	),
	file_stems_of_files_with_this_or_no_ext( T, Ext, TStems ).
