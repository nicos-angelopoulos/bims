copy_stream_on_stream( InStream, OutStream ) :-
	peek_code( InStream, Code ),
	( at_end_of_stream( InStream ) -> 
		true
		;
		get_code( InStream, Code ),
		copy_stream_on_stream( Code, InStream, OutStream )
	).

copy_stream_on_stream(-1, _In, Out) :- !,flush_output(Out).
copy_stream_on_stream(InCode, In, Out) :- 
	put_code(Out, InCode), 
	get_code(In, OutCode), 
	copy_stream_on_stream(OutCode, In, Out).

