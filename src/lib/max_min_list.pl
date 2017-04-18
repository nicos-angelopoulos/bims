%% max_min_list( +List, -Max, -Min ).
%
% Find the maximum and the minimum of a list of numbers in one pass.
%
%@author nicos angelopoulos
%@version  0.1 2014/5/7
%
max_min_list( [H|T], Max, Min ) :-
	max_min_list_1( T, H, H, Max, Min ).

max_min_list_1( [], Max, Min, Max, Min ).
max_min_list_1( [H|T], CurMax, CurMin, Max, Min ) :-
	( H > CurMax ->
		NxMax is H,
		NxMin is CurMin
		;
		NxMax is CurMax,
		( H < CurMin ->
			NxMin is H 
			;
			NxMin is CurMin
		)
	),
	max_min_list_1( T, NxMax, NxMin, Max, Min ).
