mould_vars_list( [], [] ).
mould_vars_list( [_H|T], [_FrH|FrT] ) :-
	mould_vars_list( T, FrT ).
