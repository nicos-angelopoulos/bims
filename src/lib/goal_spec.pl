%% goal_spec( +ModG, -ModSpec ). 
%% goal_spec( -ModG, +ModSpec ). 
%
% Use functor/3 on possibly module prepended Goals and Specs.
%
%==
% ?- goal_spec( data:data_file(x), Spec ).
% Spec = data:data_file/1.
% ?- goal_spec( data_file(y), Spec ).
% Spec = data_file/1.
% ?- goal_spec( G, data:data_file/1 ).
% G = data:data_file(_G1259).
%==
% @author nicos angelopoulos
% @version  0.1 2014/9/14
%
goal_spec( Goal, Spec ) :-
	var( Spec ),
	!,
	\+ var( Goal ),
	goal_spec_1( Goal, Spec ).
goal_spec( Mod:Goal, Mod:Pname/Arity ) :-
	!,
	functor( Goal, Pname, Arity ).
goal_spec( Goal, Pname/Arity ) :-
	functor( Goal, Pname, Arity ).

goal_spec_1( Mod:Goal, Mod:Pname/Arity ) :-
	!,
	functor( Goal, Pname, Arity ).
goal_spec_1( Goal, Pname/Arity ) :-
	functor( Goal, Pname, Arity ).
