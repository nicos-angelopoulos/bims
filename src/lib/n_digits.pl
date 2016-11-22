
% :- use_module( library(term_type) ). % term_codes/2 % from bims avoid dependency on other libs.
:- requires( codes_n_digits/3 ).

%% n_digits( +N, +In, -Out ).
%
%  A bimorphic predicate that convert In to Out that has excactly 
%  N digits. (Digits as in, if shorter In is padded leftwise by 0s).
%  Out is always an atom, but In can be an atom or a number.
%  The predicate first casts to codes and then calls codes_n_digits/3.
%  The order is arranged for meta calls on In - Out lists.
%  
% ==
%  ?- maplist( n_digits(2), ['1',2,'3'], Doubles ).
%  Doubles = ['01', '02', '03'].
% ==
% @author nicos angelopoulos
% @version  0.1 2014/03/17
% @see codes_n_digits/3
%
n_digits( N, In, Out ) :-
	% term_codes( In, Codes ), % for pack(bims) use:
	name( In, Codes ),
	codes_n_digits( Codes, N, OutCodes ),
	atom_codes( Out, OutCodes ).
