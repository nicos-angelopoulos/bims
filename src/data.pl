% basic predicates for data manipulation

%% bims_data_arity( -Arity ).
%  Arity is the arity of data:data/Arity predicate curently in memory.
%
bims_data_arity( Arity ) :-
	findall( A, current_predicate(data:data/A), As ),
	length( As, AsLen ),
	CapLen is min(AsLen,2),
	bims_data_arity( CapLen, As, Arity ).

bims_data_arity( 0, _, _Arity ) :-
	throw( fixme(no_data_in_memory_cannot_locate_data_arity) ).
bims_data_arity( 1, As, Arity ) :-
	As = [Arity].
bims_data_arity( 2, As, _Arity ) :-
	throw( fixme(multiple_data_preds_in_memory(As)) ).
