:- ensure_loaded( first_matching_term_on_stream).

term_in_file_to_file_contiguous_counts( FileIn, Term, CountsFile, Stays ) :-
	open( FileIn, read, Stream ),
	open( CountsFile, write, CntSt ),
	open( Stays, write, StSt ),
	read( Stream, ReadTerm ), 
	first_matching_term_on_stream( ReadTerm, Stream, Term, First ),
	term_in_stream_contiguous_counts( First, Stream, Term, First-1, CntSt, StSt ),
	close( Stream ),
	close( CntSt ),
	close( StSt ).

term_in_stream_contiguous_counts( end_of_file, _Stream, _Term, Last, CntSt, StSt ) :-
	!,
	writeq( CntSt, Last ), write( CntSt, '.' ), nl( CntSt ),
	Last = _LastTrm-LastCount,
	writeq( StSt, LastCount ), write( StSt, '.' ), nl( StSt ).
term_in_stream_contiguous_counts( ReadTermIn, Stream, Term, Acc, CntSt, StSt ) :-
	( \+ ReadTermIn = Term ->
		NxAcc = Acc
		;
		record_contiguous_count( Acc, ReadTermIn, CntSt, StSt, NxAcc )
	),
	read( Stream, NxReadTerm ), 
	term_in_stream_contiguous_counts( NxReadTerm, Stream, Term, NxAcc, CntSt, StSt ).

% record_contiguous_count( [], ReadTerm, CntSt, [ReadTerm1] ).
record_contiguous_count( CurTrm-CurCount, ReadTerm, CntSt, StSt, NxCur ) :-
	( \+ \+ CurTrm = ReadTerm ->
		NxCurCount is CurCount + 1
		;
		NxCurCount is 1,
		% to_simpler_cart( CurTrm, SimplerTrm ),
		writeq( CntSt, CurTrm-CurCount), write( CntSt, '.' ), nl( CntSt ),
		writeq( StSt, CurCount ), write( StSt, '.' ), nl( StSt )
	),
	NxCur = ReadTerm-NxCurCount.
