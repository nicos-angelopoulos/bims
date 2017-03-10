:- dynamic if_flag/1.

if( Id, X, Y, _Z ) :- 
	call( X ),
	( if_flag(Id) -> 
		true
		;
		assert( if_flag(Id)) 
	),
	call(Y).
if( Id, _X, _Y, Z ) :- 
	( if_flag(Id) -> 
		retract(if_flag(Id)),
		fail
		;
		call( Z )
	).
