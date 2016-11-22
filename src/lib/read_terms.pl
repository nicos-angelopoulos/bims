read_terms( Filename, Terms ) :-
	open( Filename, read, Stream ),
	read( Stream, Term ),
	read_stream_terms( Term, Stream, Terms ),
	close( Stream ).

read_stream_terms( end_of_file, _Stream, [] ) :-
	!.
read_stream_terms( Term, Stream, [Term|TTerms] ) :-
	read( Stream, NxTerm ),
	read_stream_terms( NxTerm, Stream, TTerms ).
