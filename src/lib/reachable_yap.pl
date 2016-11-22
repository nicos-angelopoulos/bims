:- module( reach, [reachable/3] ).

:- ensure_loaded( library(lists) ).		% append/3.
:- ensure_loaded( library(ordsets) ).		% ord_union/4.

reachable(Initial, Graph, Reachable) :-
	reachable([Initial], Graph, [Initial], Reachable).

reachable([], _, Reachable, Reachable).
reachable([Q|R], Graph, Reach0, Reachable) :-
	neighbors(Q, Graph, Neighbors),
	ord_union(Reach0, Neighbors, Reach1, New),
	append(R, New, S),
	reachable(S, Graph, Reach1, Reachable).
