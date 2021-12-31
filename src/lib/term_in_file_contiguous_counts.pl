:- ensure_loaded( increase_contiguous_count ).

term_in_file_contiguous_counts( File, Term, Count ) :-
	open( File, read, Stream ),
	read( Stream, ReadTerm ), 
	term_in_stream_contiguous_counts( ReadTerm, Stream, Term, [], Count ),
	close( Stream ).

term_in_stream_contiguous_counts( end_of_file, _Stream, _Term, Acc, Counts ) :-
	!,
	Counts = Acc.
term_in_stream_contiguous_counts( ReadTermIn, Stream, Term, Acc, Counts ) :-
	( \+ ReadTermIn = Term ->
		NxAcc = Acc
		;
		increase_contiguous_count( Acc, ReadTermIn, NxAcc )
	),
	read( Stream, NxReadTerm ), 
	term_in_stream_contiguous_counts( NxReadTerm, Stream, Term, NxAcc, Counts ).
