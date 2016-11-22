% exec_on_stream_redirection (esr)
esr( Goal ) :-
	( bims_bb_get( sr, off ) -> 
		true
		;
		call( Goal )
	).
