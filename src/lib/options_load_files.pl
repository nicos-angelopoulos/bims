:- lib(stoics_lib:en_list/2).

options_load_files( Opts ) :-
     findall( F, 
                    ( member( load(F), Opts ),
                      load_files( F ) ),
                    _ ),
     findall( L, 
                    ( member( load(L,LoptsEr), Opts ),
                      en_list( LoptsEr, Lopts ),
                      load_files( L, Lopts ) ),
                    _ ).
