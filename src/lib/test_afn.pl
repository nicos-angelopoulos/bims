:- ensure_loaded( afn ).		%.
:- ensure_loaded( '~/.lib.pl' ).		%.

:- multifile( file_search_path/2 ).

file_search_path( home, '$HOME').
file_search_path(demo, home(prolog(demo))).
file_search_path(prolog, prolog).

main( Fname ) :-
	afn( demo(mydemo), Fname ).
