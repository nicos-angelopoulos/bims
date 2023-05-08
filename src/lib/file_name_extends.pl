
:- lib(stoics_lib:en_list/2).

/**  file_name_extends( File, ExtS, ExtFile ).

     Extend a filename with one of a possibly many extensions in ExtS. 
     The first file that exists and matches is returned.
     The assumed mode is set to read, and file_name_extends/4 is called.

*/

file_name_extends( File, Ext, ExtFile ) :-
     file_name_extends( File, Ext, read, ExtFile ).

/**  file_name_extends( File, ExtS, Mode, ExtFile ).

     As in file_name_extends/3, however Mode is used to disambiguate, 
     if no extension in ExtS makes File to an existing ExtFile.
     If Mode is =|read|= then and File has no extension of its own,
     the first extension is added to produce ExtFile.

*/
file_name_extends( File, _ExtIn, _Mode, File ) :-
     exists_file( File ),
	!.
file_name_extends( File, ExtIn, Mode, Extended ) :-
     en_list( ExtIn, Exts ),
     extends_file_name( Exts, File, Exts, Mode, Extended ).

extends_file_name( [], File, [Ext|_], Mode, Extended ) :-
     Mode \== read,
     file_name_extension( Stem, _FExt, File ),
     ( File == Stem -> 
          file_name_extension( File, Ext, Extended )
          ;
          File = Extended
     ).
extends_file_name( [H|_T], File, _Exts, _Mode, Extended ) :-
     file_name_extension( File, H, Extended ),
     exists_file( Extended ).
extends_file_name( [_H|T], File, Exts, Mode, Extended ) :-
     extends_file_name( T, File, Exts, Mode, Extended ).
