:- requires( elm ).
:- elm( 'auxil/data_elm_location' ).

ensurel_data_elm_location( Dataset ) :-
	data_elm_location( Dataset, Location ),
	elm( Location ).
