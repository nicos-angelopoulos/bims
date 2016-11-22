
:- requires( directory_files/2 ).
:- requires( delete/3 ).

% :- ensure_loaded( library(lists) ).	% delete/3.
% :- ensure_loaded( library(system) ).	% directory_files/2.

directory_files_nodots( Dir, Entries ) :-
	directory_files( Dir, AllEntries ),
	delete( AllEntries, '.', NoSingleEntries ),
	delete( NoSingleEntries, '..', Entries ).
