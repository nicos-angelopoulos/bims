%% en_list( +Term, -Listed ).
%  en_list( +Term, -Listed, +Opts ).
%
% Ensure that Term is either a list of things or 
% a non-var term that is wrapped to a singleton list.
% If In is a variable a ball is thrown.
%
% Opts are passed to error handlers.
% 
%==
% ?- en_list( x(y), Opts ).
% Opts = [x(y)].
%
% ?- en_list( [x,y], Opts ).
% Opts = [x, y].
%
% % assuming you have pack(pack_errors) installed:
% ?- en_list( X, L ).
% ERROR: stoics_lib:en_list/3: Ground argument expected at position: 1,  (found: _778)
% ?- en_list( X, L, bar/1 ).
% ERROR: stoics_lib:en_list/3: Ground argument expected at position: 1,  (found: _88)
% ERROR: Trail: [bar/1]
%==
% 
%@author  nicos angelopoulos
%@version   0.2   2016/12/10
%@version   0.3   2018/10/12,  added Opts and proper error via pack_errors
%
en_list( In, Listed ) :-
    en_list( In, Listed, [] ).

en_list( In, Listed, _Opts ) :-
	is_list( In ),
	!,
	Listed = In.
en_list( In, Listed, _Opts ) :-
	\+ var( In ),
	!,
	Listed = [In].
en_list( Else, _Listed, Args ) :-
    ( is_list(Args) -> Opts = Args; Opts = [Args] ),
	throw( pack_error(arg_ground(1,Else),[stoics_lib:en_list/3|Opts]) ).
