:- ensure_loaded( library(lists) ).		% append/3.

/** break_on_list( +List, +Partial, -Left, -Right ).

Breaks a List at the sublist Partial, producing the Left and Right 
parts.

==
?- break_list_on_list( [a,b,c,d], [b,c], L, R ).
L = [a],
R = [d].
==

@author nicos angelopoulos
@version  0.2 2016/12/13, added to stoics_lib

*/
break_list_on_list( [X|Xs], [X|Ys], [], Rs ) :-
	append( Ys, Rs, Xs ),
	!.
break_list_on_list( [X|Xs], Ys, [X|Ls], Rs ) :-
	break_list_on_list( Xs, Ys, Ls, Rs ).
