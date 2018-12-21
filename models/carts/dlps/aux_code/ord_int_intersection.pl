/*
     A specialised version of ord_intersection that 
     works only for integer sets. The abbreviation 
     Low-High, is recognised as denoting Low,...,High.

     ord_int_intersection( Short, Long, Intersection, SminusL ) :-
     ord_int_intersection( Short, Long, Intersection, SminusL, LengthOfIntersection, Length of SminusL ) :-

     ord_int_intersection( [2-4], [1-3], [2,3], [4] ).
     ord_int_intersection( [2-7,9,10], [1-6,9-11], [2-6,9,10], [7] ).
     ord_int_intersection( [2-5,9-18], [1-10,12-14,17,24-26], [2-5,9,10,12-14,17],  [11,15,16,18] ).

     ord_int_intersection( [1-12,15-23], [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23], A, B ).
        ?- ord_int_intersection([5,24,30-32,37,40,43-50,52,54,55,57,58,60],[1-29,33-44,47-49,51-53,55,56,58,59], A, B, C, D ).
     A = [5,24,37,40,43,44,47-49,52,55,58],
     B = [30-32,45,46,50,54,57,60],
     C = 12,
     D = 9 ?

*/

ord_int_intersection( Short, Long, Sc, SL ) :-
     ord_int_intersection( Short, Long, 0, 0, Sc, SL, _, _ ).

ord_int_intersection( Short, Long, Sc, SL, LgSc, LgSL ) :-
     ord_int_intersection( Short, Long, 0, 0, Sc, SL, LgSc, LgSL ).
     
ord_int_intersection( [], _Long, LgSc, LgSL, [], [], LgSc, LgSL ).
ord_int_intersection( [ShLow-ShHgh|ShT], Long, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     !,
     ord_int_intersection_range( Long, ShLow, ShHgh, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ).
ord_int_intersection( [ShElm|ShT], Long, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     ord_int_intersection_elem( Long, ShElm, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ).

ord_int_intersection_range( [LnLow-LnHgh|LnT], ShLow, ShHgh, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     !,
     compare( LowsOp, ShLow, LnLow ),
     ord_int_intersection_range_lows( LowsOp, LnLow, LnHgh, LnT, ShLow, ShHgh, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ).
ord_int_intersection_range( [LnElm|LnT], ShLow, ShHgh, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     compare( LowOp, ShLow, LnElm ),
     ( LowOp = (>) ->
          NxSh = [ShLow-ShHgh|ShT],
          NxLn = LnT,
          Sc = TSc, NxSc is AcSc,
          SL = TSL, NxSL is AcSL
          ;
          ( LowOp = (=) ->
               NxShLow is ShLow + 1,
               ord_int_range_list( NxShLow, ShHgh, ShT, NxSh ),
               NxLn = LnT,
               Sc = [ShLow|TSc], NxSc is AcSc + 1,
               SL = TSL, NxSL is AcSL
               ;
               compare( HghOp, ShHgh, LnElm ),
               ( HghOp = (>) ->
                    NxShLow is LnElm + 1,
                    ord_int_range_list( NxShLow, ShHgh, ShT, NxSh ),
                    NxLn = LnT,
                    Sc = [LnElm|TSc], NxSc is AcSc + 1,
                    NxtShHgh is LnElm - 1,
                    ord_int_range_list( ShLow, NxtShHgh, TSL, AddLg, SL ),
                    NxSL is AcSL + AddLg
                    ;
                    ( HghOp = (=) ->
                         NxSh = ShT, 
                         NxLn = LnT,
                         Sc = [ShHgh|TSc], NxSc is AcSc + 1,
                         NxtShHgh is ShHgh - 1,
                         ord_int_range_list( ShLow, NxtShHgh, TSL, AddLg, SL ),
                         NxSL is AcSL + AddLg
                         ;
                         NxSh = ShT,
                         NxLn = [LnElm|LnT],
                         Sc = TSc, NxSc is AcSc,
                         SL = [ShLow-ShHgh|TSL],
                         NxSL is AcSL + (ShHgh - ShLow + 1)
                    )
               )
          )
     ),
     ord_int_intersection( NxSh, NxLn, NxSc, NxSL, TSc, TSL, LgSc, LgSL ).
               
ord_int_intersection_range_lows( <, LnLow, LnHgh, LnT, ShLow, ShHgh, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     ( LnLow > ShHgh ->
          NxSh = ShT,
          NxLn = [LnLow-LnHgh|LnT],
          Sc = TSc, NxSc is AcSc,
          SL = [ShLow-ShHgh|TSL],
          NxSL is AcSL + (ShHgh-ShLow+1)
          ;
          compare( HghsOp, ShHgh, LnHgh ),
          ( HghsOp = (>) ->
               NxShLow is LnHgh + 1,
               ord_int_range_list( NxShLow, ShHgh, ShT, NxSh ),
               NxLn = LnT,
               Sc = [LnLow-LnHgh|TSc], NxSc is AcSc + (LnHgh-LnLow+1),
               SmLHgh is LnLow - 1,
               ord_int_range_list( ShLow, SmLHgh, TSL, AddLg, SL ),
               NxSL is AcSL + AddLg
               ;
               ( HghsOp = (=) ->
                    NxSh = ShT,
                    NxLn = LnT,
                    Sc = [LnLow-LnHgh|TSc], NxSc is AcSc + (LnHgh-LnLow+1),
                    SmLHgh is LnLow - 1,
                    ord_int_range_list( ShLow, SmLHgh, TSL, AddLg, SL ),
                    NxSL is AcSL + AddLg
                    ;
                    NxSh = ShT,
                    NxLnLow is ShHgh + 1,
                    ord_int_range_list( NxLnLow, LnHgh, LnT, NxLn ),
                    ord_int_range_list( LnLow, ShHgh, TSc, AddLgSc, Sc ),
                    NxSc is AcSc + AddLgSc,
                    SmLHgh is LnLow - 1,
                    ord_int_range_list( ShLow, SmLHgh, TSL, AddLgSL, SL ),
                    NxSL is AcSL + AddLgSL
               )
          )
     ),
     ord_int_intersection( NxSh, NxLn, NxSc, NxSL, TSc, TSL, LgSc, LgSL ).
ord_int_intersection_range_lows( =, LnLow, LnHgh, LnT, ShLow, ShHgh, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     compare( HghsOp, ShHgh, LnHgh ),
     ( HghsOp = (>) ->
          NxShLow is LnHgh + 1,
          ord_int_range_list( NxShLow, ShHgh, ShT, NxSh ),
          NxLn = LnT,
          Sc = [LnLow-LnHgh|TSc], NxSc is AcSc + (LnHgh-LnLow+1),
          SL = TSL, NxSL is AcSL
          ;
          ( HghsOp = (=) ->
               NxSh = ShT,
               NxLn = LnT,
               Sc = [LnLow-LnHgh|TSc], NxSc is AcSc + (LnHgh-LnLow+1),
               SL = TSL, NxSL is AcSL
               ;
               NxSh = ShT,
               NxLnLow is ShHgh + 1,
               ord_int_range_list( NxLnLow, LnHgh, LnT, NxLn ),
               Sc = [ShLow-ShHgh|TSc], NxSc is AcSc + (ShHgh-ShLow+1),
               SL = TSL, NxSL is AcSL
          )
     ),
     ord_int_intersection( NxSh, NxLn, NxSc, NxSL, TSc, TSL, LgSc, LgSL ).
ord_int_intersection_range_lows( >, _LnLow, LnHgh, LnT, ShLow, ShHgh, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     compare( HghsOp, ShHgh, LnHgh ),
     ( ShLow > LnHgh ->
          NxSh = [ShLow-ShHgh|ShT],
          NxLn = LnT,
          Sc = TSc, NxSc is AcSc,
          SL = TSL, NxSL is AcSL
          ;
          ( HghsOp = (>) ->
               NxShLow is LnHgh + 1,
               ord_int_range_list( NxShLow, ShHgh, ShT, NxSh ),
               NxLn = LnT,
               % Sect = [ShLow-LnHgh|TSect],
               ord_int_range_list( ShLow, LnHgh, TSc, AddLg, Sc ),
               NxSc is AcSc + AddLg,
               SL = TSL, NxSL is AcSL
               ;
               ( HghsOp = (=) ->
                    NxSh = ShT,
                    NxLn = LnT,
                    Sc = [ShLow-LnHgh|TSc], NxSc is AcSc + (LnHgh-ShLow+1),
                    SL = TSL, NxSL is AcSL
                    ;
                    NxSh = ShT,
                    NxLnLow is ShHgh + 1,
                    ord_int_range_list( NxLnLow, LnHgh, LnT, NxLn ),
                    Sc = [ShLow-ShHgh|TSc], NxSc is AcSc + (ShHgh-ShLow+1),
                    SL = TSL, NxSL is AcSL
               )
          )
     ),
     ord_int_intersection( NxSh, NxLn, NxSc, NxSL, TSc, TSL, LgSc, LgSL ).

ord_int_intersection_elem( [], ShElm, ShT, LgSc, AcSL, [], [ShElm|ShT], LgSc, LgSL ) :-
     LgSL is AcSL + 1.
ord_int_intersection_elem( [LnLow-LnHgh|LnT], ShElm, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     !,
     compare( LoOp, ShElm, LnLow ),
     ( LoOp = (>) ->
          compare( HgOp, ShElm, LnHgh ),
          ( HgOp = (>) ->
               NxSh = [ShElm|ShT],
               NxLn = LnT,
               Sc = TSc, NxSc is AcSc,
               SL = TSL, NxSL is AcSL
               ;
               ( HgOp = (=) ->
                    NxSh = ShT,
                    NxLn = LnT,
                    Sc = [ShElm|TSc], NxSc is AcSc + 1,
                    SL = TSL, NxSL is AcSL
                    ;
                    NxSh = ShT,
                    NxLnLow is ShElm + 1,
                    ord_int_range_list( NxLnLow, LnHgh, LnT, NxLn ),
                    Sc = [ShElm|TSc], NxSc is AcSc + 1,
                    SL = TSL, NxSL is AcSL
               )
          )
          ;
          ( LoOp = (=) ->
               NxSh = ShT,
               NxLnLow is ShElm + 1,
               ord_int_range_list( NxLnLow, LnHgh, LnT, NxLn ),
               Sc = [ShElm|TSc], NxSc is AcSc + 1,
               SL = TSL, NxSL is AcSL
               ;
               NxSh = ShT,
               NxLn = [LnLow-LnHgh|LnT],
               Sc = TSc, NxSc is AcSc,
               SL = [ShElm|TSL], NxSL is AcSL + 1
          )
     ),
     ord_int_intersection( NxSh, NxLn, NxSc, NxSL, TSc, TSL, LgSc, LgSL ).
ord_int_intersection_elem( [LnElm|LnT], ShElm, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     compare( ElmOp, ShElm, LnElm ),
     ord_int_intersection_elems_op( ElmOp, LnElm, LnT, ShElm, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ).

ord_int_intersection_elems_op( >, _LnElm, LnT, ShElm, ShT, AcSc, AcSL, Sc, SL, LgSc, LgSL ) :-
     ord_int_intersection( [ShElm|ShT], LnT, AcSc, AcSL, Sc, SL, LgSc, LgSL ).
ord_int_intersection_elems_op( =, _LnElm, LnT, ShElm, ShT, AcSc, AcSL, [ShElm|TSc], SL, LgSc, LgSL ) :-
     NxSc is AcSc + 1,
     ord_int_intersection( ShT, LnT, NxSc, AcSL, TSc, SL, LgSc, LgSL ).
ord_int_intersection_elems_op( <, LnElm, LnT, ShElm, ShT, AcSc, AcSL, Sc, [ShElm|TSL], LgSc, LgSL ) :-
     NxSL is AcSL + 1,
     ord_int_intersection( ShT, [LnElm|LnT], AcSc, NxSL, Sc, TSL, LgSc, LgSL ).

ord_int_range_list( Low, Hgh, Tail, List ) :-
     Mark is Low + 1,
     compare( Op, Mark, Hgh ),
     ord_int_range_list_op( Op, Low, Hgh, Tail, _, List ).

ord_int_range_list( Low, Hgh, Tail, AddLength, List ) :-
     Mark is Low + 1,
     compare( Op, Mark, Hgh ),
     ord_int_range_list_op( Op, Low, Hgh, Tail, AddLength, List ).

ord_int_range_list_op( < , Low, Hgh, Tail, Length, [Low-Hgh|Tail] ) :-
     Length is Hgh - Low + 1.
ord_int_range_list_op( = , Low, Hgh, Tail, 2, [Low,Hgh|Tail] ).
ord_int_range_list_op( > , Low, _Hgh, Tail, 1, [Low|Tail] ).
