:- dynamic if_count/1.
:- assert( if_count(1) ).

body_expansion( (A->B;C), (D->E;F) ) :-
	body_expansion( A, D ),
	body_expansion( B, E ),
	body_expansion( C, F ).
body_expansion( (A,B), (C,D) ) :-
	body_expansion( A, C ),
	body_expansion( B, D ).
body_expansion( (A;B), (C;D) ) :-
	body_expansion( A, C ),
	body_expansion( B, D ).
body_expansion( if(X,Y,Z), if(Id,X,Y,Z) ) :-
	if_count(Id),
	retractall( if_count(_) ),
	NxId is Id + 1,
	asserta( if_count(NxId) ).
body_expansion( A, A ).

term_expansion( if(X,Y,Z), if(Id,X,Y,Z) ) :-
	if_count(Id),
	retractall( if_count(_) ),
	NxId is Id + 1,
	asserta( if_count(NxId) ).

term_expansion( (A:-B), (A:-C) ) :-
	body_expansion( B, C ).
