%% select_all( +List, +Elem, -Select, -Rem ).
% 
% Select all elements of List that are term subsumed (subsumes_term/2) by Elem.
% Rem is the non selected elements of List
% 
% works on Swi have n't tested Yap...
% 
% ==
% select_all( [a(b),b(c),a(b),d(a),a(c)], a(A), Sel, Rem ).
% Sel = [a(b), a(b), a(c)],
% Rem = [b(c), d(a)].
% 
% select_all( [a(b),b(c),a(b),d(a),a(c)], a(b), Sel, Rem ).
% Sel = [a(b), a(b)],
% Rem = [b(c), d(a), a(c)].
% 
% ==
% 
% @author nicos angelopoulos
% @version  0.2 2014/4/7
%
% :- ensure_loaded( library(terms) ).

select_all( [], _Elem, [], [] ).
select_all( [H|T], Elem, Sel, NonSel ) :-
     subsumes_term( Elem, H ),
     !,
     Sel = [H|TSel],
     select_all( T, Elem, TSel, NonSel ).
select_all( [H|T], Elem, Sel, [H|TNonSel] ) :-
     select_all( T, Elem, Sel, TNonSel ).
