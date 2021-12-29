:- lib(lists).  % flatten/2

write_list_with_line_numbers( [], _Number, _Spaces ).
write_list_with_line_numbers( [H|T], Number, Spaces ) :-
	% flatten( ["~t~d~",Spaces,"| : "], FormatThis ),
	atomic_list_concat( ['~t~d~',Spaces,'| : '], FormatThisAtm ),
	format( FormatThisAtm, [Number] ),
	NxN is Number + 1,
	write( H ), nl,
	write_list_with_line_numbers( T, NxN, Spaces ).
