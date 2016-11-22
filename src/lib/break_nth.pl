% break_nth( Nth, List, Left, Right ) :-
% List is split on Nth Position, into Left, and Right Parts
% First element position  is number 1. Nth element is last element in Left.
% eg ?- break_nth( 0, [a,b,c], L, R ).  L=[], R=[a,b,c]
%    ?- break_nth( 1, [a,b,c], L, R ).  L=[a], R=[b,c]
%    ?- break_nth( 3, [a,b,c], L, R ).  L=[a,b,c], R=[].
%    ?- break_nth( 4, [a,b,c], L, R ).  error

break_nth( N, List, Left, Right ) :-
	G = break_nth/4,
	( N < 0 ->
     	write( user_error, expected_a_natural_number(N,G) ),
		nl( user_error ),
		abort
		;
		( N = 0 ->
			Left = [],
			Right = List
			;
     		length( List, Length ),
			( Length =:= N ->
				Left = List, Right = []
				;
				( Length < N ->
					write( user_error, list_of_insufficient_length(legth(Length),limit(N),G) ), nl( user_error ), abort
					;
     				break_nth_1( N, List, Left, Right )
				)
			)
		)
	).

break_nth_1( 1, [H|T], [H], T ) :- !.
% break_nth_for_list_1( 1, [X|Xs], [X], Xs ) :- !.
break_nth_1( N, [X|Xs], [X|Ls], Right ) :-
     N1 is N - 1,
     break_nth_1( N1, Xs, Ls, Right ).
