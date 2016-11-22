pop_first_three_numbers_from_file( File, First, Sec, Third ) :-
	open( File, read, InStream ),
	read_file_of_numbers_to_list( InStream, ListOfNums ),
	close( InStream ),
	( ListOfNums = [First,Sec,Third|T]
		->     true
		;
			write( ['error >> in:pop_first_three_numbers_from_file/4, either ', File, ' has not enough numbers, or last 3 parameters are instatiated to the wrong atoms. Now failing .... (file not changed)'] ),
			fail
	),
	open( File, write, OutStream ),
	write_list_to_single_stream( OutStream, T ),
	close( OutStream ).

write_list_to_single_stream( Stream, [H1,H2,H3|T] ) :-
        write( Stream, H1 ), write( Stream, ' ' ),
        write( Stream, H2 ), write( Stream, ' ' ),
        write( Stream, H3 ), write( Stream, ' ' ),
        nl(Stream),
        !,
        write_list_to_single_stream( Stream, T ).
write_list_to_single_stream( _Stream, [] ).

read_file_of_numbers_to_list( InStream, ListOfNums ) :-
	read_number( InStream, FirstNumb ), 
	!,
	ListOfNums = [FirstNumb|TailOfNums],
	read_file_of_numbers_to_list( InStream, TailOfNums ).
read_file_of_numbers_to_list( _AnyStream, [] ).

read_number( InStream, _Number ) :-
        at_end_of_stream( InStream ),
        !, 
        fail.

read_number( InStream, Number ) :-
        get0( InStream, Char ),
        ( (Char > 47, Char < 58) -> 
                Acc is Char - 48,
                read_number_1( InStream, Acc, Number )
                                 ;
                read_number( InStream, Number )
        ).

read_number_1( InStream, Acc, Number ) :-
        get0( InStream, Char ),
        !,
        ( (Char > 47, Char < 58) -> 
                NewAcc is (Acc * 10) + (Char-48),
                read_number_1( InStream, NewAcc, Number )
                                 ;
                Number is Acc 
        ).

