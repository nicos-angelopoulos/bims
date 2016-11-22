:- ensure_loaded( library(lists) ).		% memberchk/2.
:- ensure_loaded( library(system) ).		% mktemp/2.

tmp_in_options( Opts, All, Tmp ) :-
	( memberchk(tmp(Tmp),Opts) ->
		true
		;
		memberchk( stem(Stem), All ),
		atom_concat( Stem, '_XXXXXX', Tmplate ),
		mktemp(Tmplate,Tmp)
	).
