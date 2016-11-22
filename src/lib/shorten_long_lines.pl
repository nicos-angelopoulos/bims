:- ensure_loaded( library(lists) ).		% reverse/2, append/3.

/*
test :-
	shorten_long_lines( ["	ABCD EFGH IJKL MNOP QSTU VWXY Z","QWERTYUIOPAS DFGHJKLZXCVBNM","WHICH"], 10, 3,  "\t", S ),
	write( S ), nl,
	write_s( S ).

write_s( [] ).
write_s( [H|T] ) :-
	atom_codes( HAt, H ), write( HAt ), nl,
	write_s( T ).
	*/

% shorten_long_lines( Lines, Length, Back, Add, Shorts ) :-
% Length, maximum length for chopping Lines to produce Shorts
% Todo: parametrise where back windowing stops.
%       currently tabs count for single character.
shorten_long_lines( Lines, Length, Ptc, Back, Add, Shorts ) :-
	( Back < Length -> 
		true
		; 
		write( user_error, 'shorten_long_lines/6, arg 4 cannot be smaller than arg 2' ), 
		nl( error ), abort
	),
	shorten_long_lines( Lines, Length, Ptc, Back, Add, [], Shorts ).

shorten_long_lines( [], _Length, _Ptc, _Back, _Add, Shorts, Shorts ).
shorten_long_lines( [H|T], Length, Ptc, Back, Add, Acc, Shorts ) :-
	shorten_long_line( H, Length, Ptc, Back, Add, HShLns ),
	append( Acc, HShLns, NxAcc ),
	shorten_long_lines( T, Length, Ptc, Back, Add, NxAcc, Shorts ).

shorten_long_line( Line, Length, Ptc, Back, Add, Shorts ) :-
	collect_leading_whitespaces( Line, Leads ),
	append( Leads, Add, AllLeads ),
	reverse( AllLeads, PrvRevLeads ),
	length( PrvRevLeads, PrvLgLds ),
	( PrvLgLds < Length  ->
		% RevLeads = PrvRevLeads
		reverse( Ptc, Ctp ),
		append( PrvRevLeads, Ctp, RevLeads )
		;
		reverse( Ptc, RevLeads )
	),
	shorten_long_line_1( Line, 1, Length, Ctp, Back, RevLeads, [], Shorts ).

shorten_long_line_1( [], N, _Length, _Ctp, _Back, _Leads, Acc, Shorts ) :-
	( N =:= 1 -> Shorts = [[]] ; reverse(Acc,Short),Shorts=[Short] ).
shorten_long_line_1( [H|T], N, Length, Ctp, Back, Leads, Acc, Shorts ) :-
	( N =:= Length ->
		% NxN is LgLds,
		move_window_in_chopped_line( Back, [H|Acc], Ctp, Leads, [], Short, NxAcc ),
		length( NxAcc, NxNminus ),
		NxN is NxNminus + 1,

		% NxAcc = Leads,
		% reverse( [H|Acc], Short ),
		Shorts = [Short|TShorts]
		;
		NxN is N + 1,
		NxAcc = [H|Acc],
		TShorts = Shorts
	),
	shorten_long_line_1( T, NxN, Length, Ctp, Back, Leads, NxAcc, TShorts ).

move_window_in_chopped_line( 0, RevLine, Ctp, Leads, Acc, Short, Carry ) :-
	!,
	Carry = Leads, 
	reverse( RevLine, Line ),
	reverse( Ctp, Ptc ),
	append( Acc, Ptc, AccPtc ),
	append( Line, AccPtc, Short ).
move_window_in_chopped_line( B, [H|T], Ctp, Leads, Acc, Short, Carry ) :-
	( is_space_or_tab(H) -> 
		append( Ctp, [H|T], CtpRevShort ),
		reverse( CtpRevShort, Short ),
		% reverse( [H|T], Short ),
		reverse( Acc, RevAcc ),
		append( RevAcc, Leads, Carry )
		;
		NxB is B - 1,
		move_window_in_chopped_line( NxB, T, Ctp, Leads, [H|Acc], Short, Carry )
	).

collect_leading_whitespaces( [], [] ).
collect_leading_whitespaces( [H|T], Leads ) :-
	( is_space_or_tab(H) ->
		Leads = [H|TLeads],
		collect_leading_whitespaces( T, TLeads )
		;
		Leads = []
	).

is_space_or_tab( 32 ) :- !.
is_space_or_tab( 09 ).
