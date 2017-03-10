:- lib( select_all/4 ).

%% select_first( +List, +Elem, -Rem ).
%
% An idiom of select_all/4 which unfolds to select_all( List, Elem, [H|_], Rem ), H = Elem.
%==
% ?- select_first( [dbg(t),dbg(f),etc(x)], dbg(W), Rem ).
% W = t,
% Rem = [etc(x)].
%==
% @author nicos angelopoulos
% @version  0.1 2014/4/7
%
select_first( List, Elem, Rem ) :-
	select_all( List, Elem, [H|_], Rem ),
	H = Elem.
