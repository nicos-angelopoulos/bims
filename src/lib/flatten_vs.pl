%%%%%%%%
% flatten( +NestedLists, ?FlatList ) :-
% FlatList is the list of all not list elements of 
% the lists in NestedLists, in depth first, left to right
% order, from o'keefe's book p. 97-98
%%%%%%%
% this version doesnt replace vars with empty lists.
%
flatten_vs( Nlist, Flist ) :-
	flatten_vs( Nlist, Flist, [] ).
flatten_vs( V, L0, L ) :-
	var( V ),
	!,
	L0 = [V|L].
flatten_vs( [], L0, L ) :-
	!,
	L0 = L.
flatten_vs( [H|T], L0, L ) :-
	!,
	flatten_vs( H, L0, L1 ),
	flatten_vs( T, L1, L ).
flatten_vs( Oth, [Oth|List], List ).
