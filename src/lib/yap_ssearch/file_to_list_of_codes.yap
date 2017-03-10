%%%  file+stream to_list_of_chars

file_to_list_of_codes( File, Chars ) :-
        open( File, read, Stream ),
        get0( Stream, C ),
        stream_to_list_of_codes( C, Stream, Chars ),
        close( Stream ).

stream_to_list_of_codes( -1, _Stream, Chars ) :-
	!,
	Chars = [].
stream_to_list_of_codes( C, Stream, [C|Cs] ) :-
	get0( Stream, NxC ),
	stream_to_list_of_codes( NxC, Stream, Cs ).
