:- module( ord_subset_difference, [ord_subset_difference/3] ).

ord_subset_difference( [], Diff, Diff ).
ord_subset_difference( [H|T], Set2, Diff ) :-
	ord_del_element_strict( Set2, H, Set3 ),
	ord_subset_difference( T, Set3, Diff ).

ord_del_element_strict([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element_strict(Order, Head, Tail, Element, Set).

ord_del_element_strict(<, Head, Tail, Element, [Head|Set]) :-
	ord_del_element_strict(Tail, Element, Set).
ord_del_element_strict(=, _, Tail, _, Tail).
