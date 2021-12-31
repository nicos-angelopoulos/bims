:- ensure_loaded( pl ).        % /1.
:- use_module(library(system)).  % file_exists/1.
:- use_module(library(lists) ).  % member/2.

% system independent (Yap + SICStus) absolute_file_name/2 modelled after SICStus.
afn( InFile, OutFile ) :-
     ( pl(sicstus(_S))   ->
          absolute_file_name( InFile, OutFile )
          ;
          ( (  afn_1( InFile, MidFile ),
               % pl_file_extensions_append( MidFile, OutFile ), 
               absolute_file_name( MidFile, OutFile ) ) ->  % Yap specific
               % file_exists( OutFile ) ) ->
                    true
                    ;
                    afn_1( InFile, OutFile ),
                    !
          )
     ).

afn_1( InFile, OutFile ) :-
     ( compound(InFile) ->
          InFile =.. [Name|Args],
          ( Name == library ->
               library_directory( Expansion )
               ;
               file_search_path( Name, Expansion )
          ),
          afn_1( Expansion, FlatName ),
          afns( Args, FlatArgs ),
          % OutFile =.. [FlatName|FlatArgs]
          % fixme: this is probably SWI specific
          atomic_list_concat( [FlatName,'/'|FlatArgs], '', OutFile )
          ;
          OutFile = InFile
     ).

afns( [], [] ).
afns( [H|T], [AbsH|AbsT] ) :-
     afn_1( H, AbsH ),
     afns( T, AbsT ).

pl_file_extensions_append( PlFile, PlFile ).
pl_file_extensions_append( InFile, PlFile ) :-
     member( Ext, [".pl",".yap"] ),
     atom_codes( InFile, InFileCs ),
     append( InFileCs, Ext, PlFileCs ),
     atom_codes( PlFile, PlFileCs ).
