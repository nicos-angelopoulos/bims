:- ensure_loaded( increase_contiguous_count_dl ).

spec_in_file_contiguous_counts( File, Counts ) :-
	spec_in_file_contiguous_counts( File, _, _, Counts ).

spec_in_file_contiguous_counts( File, Fnc, Art, Counts ) :-
     % assume first term is the one to be counted.
	open( File, read, In ),
	read( In, Term ),
	( var(Fnc) -> 
     	functor( Term, Fnc, Art )
		;
		true
	),
	read_to_first( Term, In, Fnc, Art, First ), 
	read( In, Read ),
	spec_in_stream_contiguous_counts( Read, First, 1, In, Fnc, Art, Counts, [] ),
	close( In ).

/*
spec_in_file_contiguous_counts( File, Fnc, Art, Counts ) :-
	open( File, read, In ),
	read( In, Term ),
	read_to_first( Term, In, Fnc, Art, First ), 
	read( In, Read ),
	spec_in_stream_contiguous_counts( Read, First, 1, In, Fnc, Art, Counts, [] ),
	close( In ).
	*/

spec_in_stream_contiguous_counts( end_of_file, Curr, Cnt, _In, _Fnc, _Art, List, Tail ) :-
	!,
	List = [Cnt-Curr|Tail].
spec_in_stream_contiguous_counts( Term, Curr, Cnt, In, Fnc, Art, List, Tail ) :-
	( functor( Term, Fnc, Art ) ->
		increase_contiguous_count_dl( Curr, Cnt, Term, NxCurr, NxCnt, List, NxList )
		;
		NxCurr = Curr, NxCnt = Cnt, NxList = List
	),
	read( In, NxTerm ),
	spec_in_stream_contiguous_counts( NxTerm, NxCurr, NxCnt, In, Fnc, Art, NxList, Tail ).

read_to_first( Term, _In, Fnc, Art, Read ) :-
	functor( Term, Fnc, Art ),
	!,
	Read = Term.
read_to_first( _Term, In, Fnc, Art, Read ) :-
	read( In, NxTerm ),
	read_to_first( NxTerm, In, Fnc, Art, Read ).
