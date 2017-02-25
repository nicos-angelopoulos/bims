%% compound( +Term, -Name, -Args ).
%
%  Tries to deal with syntax changes that allow a() as a legal term.
%
% Examples run on Swi.7 
%==
%  compound( abc, Name, Args ).
%  false.
%  
%  compound( abc(a,b,c), Name, Args ).
% Name = abc,
% Args = [a, b, c].
% 
% compound( Term, abc, [a,b,c] ).
% Term = abc(a, b, c).
% 
% compound( Term, abc, [] ).
% Term = abc().
%
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/1/10 (round about)
%
compound( Term, Name, Args ) :-
	current_predicate( compound_name_arguments/3 ),
	!,
	once( (compound(Term) ; (ground(Name),is_list(Args))) ),
	% !,
	compound_name_arguments( Term, Name, Args ).
compound( Term, Name, Args ) :-
	once( (compound(Term) ; (ground(Name),ground(Args))) ),
	Term =.. [Name,Args].
