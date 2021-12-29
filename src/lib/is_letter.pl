is_letter( Letter ) :-
	( (Letter >= 0'A, Letter =< 0'Z) ;
	  (Letter >= 0'a, Letter =< 0'z) ),
	!.
