file_stems_of_files_with_this_ext( [], _Ext, [] ).
file_stems_of_files_with_this_ext( [H|T], Ext, Stems ) :-
	( atom_concat(Stem,Ext,H) ->
		Stems = [Stem|TStems]
		;
		TStems = Stems
	),
	file_stems_of_files_with_this_ext( T, Ext, TStems ).
