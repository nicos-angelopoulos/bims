
:- ensure_loaded( library(lists) ).     % select/3
:- ensure_loaded( library(random) ).    % random/3

break_cycles_w_self( Edges, Removed, Reduced ) :-
     remove_identical_edges( Edges, NonAutoEdges ),
     break_cycles( NonAutoEdges, Removed, Reduced ).

break_cycles( Edges, Removed, Reduced ) :-
     Edges = [F-S|_R],
     break_cycles( S, [F], Edges, Removed, Reduced ).

break_cycles( Node, Path, Edges, [ToFromEdge|Rmv], Reduced ) :-
     memberchk( Node, Path ),
     !,
     % trace,
     break_cycle( [Node|Path], Edge, NewPath ), % the prefix
     Edge = A-B, ToFromEdge = B-A,
     remove_edge( Edge, Edges, NxtEdges ),
     NewPath = [Last|NewPathCont],
     break_cycles( Last, NewPathCont, NxtEdges, Rmv, Reduced ).
break_cycles( Node, Path, Edges, Rmv, Reduced ) :-
     memberchk( Node-Child, Edges ), 
     !,
     break_cycles( Child, [Node|Path], Edges, Rmv, Reduced ).
break_cycles( Node, [Last|Path], Edges, Rmv, Reduced ) :-
     !,
     remove_edge( Last-Node, Edges, NxtEdges ),
     Reduced = [Last-Node|TRed],
     break_cycles( Last, Path, NxtEdges, Rmv, TRed ).
break_cycles( _Node, [], [], [], [] ) :-
     !.
     % write( done(Node) ), nl.
break_cycles( _Node, [], Edges, Rem, Reduced ) :-
     Edges = [F-S|_R],
     break_cycles( S, [F], Edges, Rem, Reduced ).

remove_edge( Edge, Edges, NxtEdges ) :-
     select( Edge, Edges, NxtEdges ).

break_cycle( [A,B|Path], B-A, Path ).

remove_identical_edges( [], [] ).
remove_identical_edges( [X-Y|T], A ) :-
     ( X == Y -> 
          A = R
          ;
          A = [X-Y|R]
     ),
     remove_identical_edges( T, R ).
     

/* random 
break_cycle( [A|Path], Edge, NewPath ) :-
     length( [A|Path], L ),
     random( 1, L, I ),
     break_r_cycle( I, Path, A, Edge, Left, Right ),
     ( I > L / 2 -> NewPath=Left; NewPath=Right ).
     % write( broke(Edge) ), nl.

break_r_cycle( 1, [B|Path], A, B-A, [], Path ) :- !.
break_r_cycle( I, [B|Path], A, Edge, [A|R], Rem ) :-
     NxI is I - 1,
     break_r_cycle( NxI, Path, B, Edge, R, Rem ).
     */
