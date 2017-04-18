%% nth_replace( ?N, +List, ?With, ?Nth, +NewList ).
% 
% Find and replace the nth element of a list. The list with the element replaced
% is in NewList.
%
%@author Nicos Angelopoulos
%@version 0.2 2011/?/?, 2005/02/23.
%
nth_replace( I, List, With, Nth, NewList ) :-
     number(I),
     !,
     nth_replace_n( I, List, With, Nth, NewList ).
nth_replace( I, List, With, Nth, NewList ) :-
     var( I ),
     nth_replace_gen( List, With, Nth, 1, I, NewList ).

nth_replace_gen( [H|T], With, H, I, N, NewList ) :-
     N is I,
     NewList = [With|T].
nth_replace_gen( [H|T], With, Nth, I, N, [H|R] ) :-
     NxI is I + 1,
     nth_replace_gen( T, With, Nth, NxI, N, R ).

nth_replace_n( 1, List, With, Nth, NewList ) :-
	!,
	List = [Nth|T],
	NewList = [With|T].
nth_replace_n( N, [H|T], With, Nth, [H|NewT] ) :-
	NxN is N - 1,
	nth_replace_n( NxN, T, With, Nth, NewT ).
