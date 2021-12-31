
/** delete_file_if( +File ).

	Always true. File will be deleted if it exists.

    Preds within here are built-ins in SWI.
    fixme: Check for compatibility on other systems.
*/
delete_file_if( File ) :-
	( exists_file(File) -> 
		delete_file(File)
		;
		true
	).
