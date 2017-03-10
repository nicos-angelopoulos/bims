:- lib( directory_files/2 ).
:- lib( is_a_regular_file/1 ).

directory_files_regular( Dir, Regulars ) :-
	directory_files( Dir, All ),
	working_directory( Old, Dir ),
	sieve_regular_files( All, Regulars ),
	working_directory( _DirA, Old ).

sieve_regular_files( [], [] ).
sieve_regular_files( [H|T], Regulars ) :-
	% we assume inputs exist...
	( is_a_regular_file(H) ->
		Regulars = [H|Treg]
		;
		Regulars = Treg
	),
	sieve_regular_files( T, Treg ).
