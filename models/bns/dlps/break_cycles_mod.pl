
:- module( break_cycles, [break_bn_cycles/2] ).
% :- module( break_cycles, [break_bn_cycles/2,go/0] ).

:- ensure_loaded( break_cycles ).
:- ensure_loaded( library(ordsets) ).

/*
go :-
     LoopAsia = [1-[7],2-[1],3-[2,5],4-[],5-[4],6-[4],7-[3],8-[3,6]],
     break_bn_cycles( LoopAsia, Asia ),
     write( asia(Asia) ), nl.
     */

break_bn_cycles( Graph, Bn ) :-
     pa_graph_to_edges( Graph, Edges ),
     keysort( Edges, Sorted ),
     break_cycles_w_self( Sorted, Rmv, _ ),
     keysort( Rmv, RmvSorted ),
     pa_graph_remove_edges( Graph, RmvSorted, Bn ).

pa_graph_to_edges( [], [] ).
pa_graph_to_edges( [Node-Pa|T], Edges ) :-
     pa_list_to_edges( Pa, Node, Edges, Tedges ),
     pa_graph_to_edges( T, Tedges ).

pa_list_to_edges( [], _Node, Tedges, Tedges ).
pa_list_to_edges( [H|T], Node, [H-Node|Edges], Tedges ) :-
     pa_list_to_edges( T, Node, Edges, Tedges ).

pa_graph_remove_edges( [], _Rmv, [] ).
pa_graph_remove_edges( [Node-Pa|T], Rmv, [Node-RmvPa|R] ) :-
     edges_towards_node( Rmv, Node, From, RemRmv ),
     sort( From, SrtFrom ),
     % remove_from_pa_list( Pa, SrtFrom, RmvPa ),
     ord_subtract( Pa, SrtFrom, RmvPa ),
     pa_graph_remove_edges( T, RemRmv, R ).

edges_towards_node( [], _Node, [], [] ).
edges_towards_node( [To-Fr|T], Node, From, RemRmv ) :-
     compare( Op, To, Node ),
     edges_towards_node_op( Op, To, Fr, T, Node, From, RemRmv ).

edges_towards_node_op( <, To, Fr, T, Node, From, [To-Fr|R] ) :-
     edges_towards_node( T, Node, From, R ).
edges_towards_node_op( =, _To, Fr, T, Node, [Fr|Tfrom], R ) :-
     edges_towards_node( T, Node, Tfrom, R ).
edges_towards_node_op( >, To, Fr, T, _Node, [], [To-Fr|T] ).

