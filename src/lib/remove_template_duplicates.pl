
%% remove_template_duplicates( +List, -NonDups ).
%
% True iff NonDups is List with all subsequent term_template/2 unifiables 
% removed.
%==
% ?- remove_template_duplicates( [a(1),b(2),a(3),b(4)], NonDups ).
% NonDups = [a(1), b(2)].
%==
% @author nicos angelopoulos
% @version  0.1 2014/9/1
%
remove_template_duplicates( [], [] ).
remove_template_duplicates( [H|T], [H|R] ) :-
    functor( H, Name, Arity ),
    functor( Template, Name, Arity ),
	remove_template_duplicates_of_element( T, Template, Rem ),
	remove_template_duplicates( Rem, R ).

remove_template_duplicates_of_element( [], _Template, [] ).
remove_template_duplicates_of_element( [H|T], Template, Rem ) :-
	\+ \+ H = Template,
	!,
	remove_template_duplicates_of_element( T, Template, Rem ).
remove_template_duplicates_of_element( [H|T], Template, [H|Rem] ) :-
	remove_template_duplicates_of_element( T, Template, Rem ).
