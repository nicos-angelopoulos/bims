%% if( +If, +Then ).
%% if( +If, +Then, +Else ).
%
% Call If, and then THen if it succeeds or Else otherwise.
% If is called deterministically. If Else is missing, call 'true'.
% This predicate is syntactic sugar for (->;)
%==
% ?- L = [b,a], if( select(a,L,Rem), X = Rem, X = L).
% Rem = X, X = [b].
% ?- L = [b,c], if( select(a,L,Rem), X = Rem, X = L).
% L = X, X = [b, c].
% ?- L = [b,c], if( select(a,L,Rem), X = Rem ).
% L = [b, c].
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/9/10
%
if( If, Then ) :-
	if( If, Then, true ).

if( If, Then, _Else ) :-
	call( If ),
	!,
	once( call(Then) ).
if( _If, _Then, Else ) :-
	call( Else ).
