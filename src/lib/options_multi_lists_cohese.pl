:- lib(stoics_lib:en_list/2).

options_multi_lists_cohese( Opts, Fnc, Coh ) :-
	options_multi_lists_cohese_1( Opts, Fnc, [], Coh ).

options_multi_lists_cohese_1( [], _Fnc, Coh, Coh ).
options_multi_lists_cohese_1( [H|T], Fnc, Acc, Fs ) :-
	( H =.. [Fnc|Arg] -> 
		en_list( Arg, AList ),
		append( Acc, AList, NxAcc )
		;
		NxAcc = Acc
	),
	options_multi_lists_cohese_1( T, Fnc, NxAcc, Fs ).
