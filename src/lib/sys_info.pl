:-	ensure_loaded( library(system) ).
	% system/2.

sys_info( System ) :-
	pl( swi(_), shell(System,Status), system(System,Status) ),
	( Status = 0 -> StatusTerm = ok(System)
			   ;	 StatusTerm = exit(Status,System)
	),
	print_message( informational, StatusTerm ).
