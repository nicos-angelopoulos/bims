opts_select_infiles( [], [], [] ).
opts_select_infiles( [H|T], Fs, RmOpts ) :-
	( var(H) -> 
		TFs = Fs, RmOpts = [H|TRmOpts]
		;
		( H = in(File) ->
			Fs = [File|TFs], TRmOpts = RmOpts
			;
			( atom(H) ->
				Fs = [H|TFs], TRmOpts = RmOpts
				;
				TFs = Fs, RmOpts = [H|TRmOpts]
			)
		)
	),
	opts_select_infiles( T, TFs, TRmOpts ).
