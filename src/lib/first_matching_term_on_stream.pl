first_matching_term_on_stream( end_of_file, _Stream, _Term, _First ) :-
	!,
	fail.
first_matching_term_on_stream( ReadTerm, _Stream, Term, First ) :-
	\+ \+ ReadTerm = Term,
	!,
	First = ReadTerm.
first_matching_term_on_stream( _ReadTerm, Stream, Term, First ) :-
	read( Stream, NxReadTerm ),
	first_matching_term_on_stream( NxReadTerm, Stream, Term, First ).
