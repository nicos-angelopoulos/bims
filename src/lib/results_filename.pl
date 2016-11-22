:- ensure_loaded( flatten ).

results_filename( Mode, Iter, File ) :-
	atom_codes( Mode, ModeCs ),
	pob( Pob ),
	number_codes( Pob, PobCs ),
	PobCs = [0'0,0'.|FrCs],
	number_codes( Iter, IterCs ),
	datafile( library(Data) ), 
	atom_codes( Data, DataCs ),
	append( _Prefix, [A,B,C,D], DataCs ),
	flatten( ["res_","asi8_",ModeCs,"_p",FrCs,"_i",IterCs,"_d",
		[A,B,C,D]], FlatCs ),
	atom_codes( File, FlatCs ).

