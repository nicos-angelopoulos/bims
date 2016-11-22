write_list_elem_per_line_w_period( [] ).
write_list_elem_per_line_w_period( [H|T] ) :-
	write( H ), write( '.' ), nl,
	write_list_elem_per_line_w_period( T ).
