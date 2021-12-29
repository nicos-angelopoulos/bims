:- ensure_loaded(write_list_with_line_numbers).
:- dynamic(dbg_this/1).
:- lib(stoics_lib:en_list/2).
 
dbg_these( ListPrv ) :-
     en_list( ListPrv, List ),
     retractall( dbg_this(_) ),
     dbg_these_1( List ).

dbg_these_1( [] ).
dbg_these_1( [H|T] ) :-
     assert( dbg_this(H) ),
     dbg_these_1( T ).

dbg_list( N, Heading, WriteThese ) :- 
     % bb_get( dbg_these, WhichDbgs ),
     % ( memberchk(N,WhichDbgs) ->
     ( dbg_this(N) -> 
          write( [N] ), write( : ),
          write( Heading ), write( ' : ' ), nl,
          write_list_with_line_numbers( WriteThese, 1, "3" )
          ;
          true
     ).

dbg( N, Message ) :- 
     % bb_get( dbg_these, List ), % ( memberchk(N,List) ->
     ( dbg_this(N) -> 
          write_term( Message, [quoted(true)] ), write( '.' ), nl
          ;
          true
     ).
