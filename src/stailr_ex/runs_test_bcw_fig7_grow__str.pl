run_data( 1, uc, ids_split_str, rm, Data, SGl, OutF, 60000, [], 1, all ) :-
        Data = idsd( 9, bcw, Splits, Ids ),
        SGl = cart/[Splits,Ids,0.95/1],
        OutF = a0_95b1.
