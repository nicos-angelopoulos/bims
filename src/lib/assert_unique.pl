%% assert_unique( +Fact ).
%
% Retractall Fact's template before asserting term.
% Fact can be a module prepended term.
% 
%==
% ?- assert_unique( abc:letter(a) ).
% ?- abc:letter(A).
% A = a.
% ?- assert_unique( abc:letter(b) ).
% ?- abc:letter(B).
% B = b.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/9/2
% @tbd better name?
%
assert_unique( Mod:Term ) :-
	!,
    functor( Term, Name, Arity ),
    functor( Fresh, Name, Arity ),
	retractall( Mod:Fresh ),
	assert( Mod:Term ).
assert_unique( Term ) :-
    functor( Term, Name, Arity ),
    functor( Fresh, Name, Arity ),
	retractall( Fresh ),
	assert( Term ).
