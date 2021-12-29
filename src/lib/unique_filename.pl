
:- ensure_loaded( library(lists) ).     % append/3, member/2, flatten/2.
:- ensure_loaded( library(system) ).    % file_exists/1.

:- lib(stoics_lib:en_list/2).
:- lib(break_nth/4).
:- lib(file_exists/1).
:- lib(break_list_on_list/4).
:- lib(option_in_values_or_else_default/6).

/*
testo_1 :- 
     unique_filename( abc__test, Unique ),
     testo_touch( Unique ),
     unique_filename( abc__test, Second ), 
     testo_touch( Second ).
testo_2 :- 
     unique_filename( abc__test, Unique ),
     testo_touch( Unique ),
     unique_filename( abc__test, [report(false),version(n)], Second ), 
     testo_touch( Second ).
testo_3 :- 
     unique_filename( abc__test, [extension(dot)], Unique ),
     testo_touch( Unique ),
     unique_filename( abc__test, [report(false),separator(+)], Second ), 
     testo_touch( Second ).
testo_4 :- 
     unique_filename( abc__test, [extension(dot),add(testo_call,3)], Unique ),
     testo_touch( Unique ),
     unique_filename( abc__test, [report(false)], Second ), 
     testo_touch( Second ).
testo_call( Res ) :-
     pl( swi(_), testo_call_swi(Res), testo_call_yap(Res) ).
testo_call_swi( Host ) :-
     getenv( 'HOSTNAME', Host ).
testo_call_yap( Host ) :-
     environ( 'HOSTNAME', Host ).
     */

testo_touch( File ) :-
     atom_concat( 'touch ', File, Touch ),
     write( touching(File) ), nl,
     shell( Touch ).
testo_clean :- shell( 'rm abc__test*' ).

safe_unique_filename( Stream, Stream ) :-
     is_a_stream( Stream ), !.
safe_unique_filename( FileAtom, UniqueFile ) :-
     unique_filename( FileAtom, UniqueFile ).

unique_filename_defaults( Defs, RepVals ) :-
     Defs = [  version(v),
               separator('_'),
               % add(Call,Lgt),          no default adds
               report(true),
               type(file),              % else dir
               extension(null__)],  % not '' ?
     RepVals = [true,false].

unique_filename( FileAtom, UniqueFile ) :-
     unique_filename( FileAtom, UniqueFile, [] ).

% Opt in {report([true,false]),version(AtomicVrs),extension(Ext)}
% I guess we can ask for type([integer,upper,lower,letter]),
% and length of extension bits
%
unique_filename( FileIn, UnqFile, OptsIn ) :-
     unique_filename_defaults( Defs, RepVls ),
     en_list( OptsIn, Opts ),
     append( Opts, Defs, All ),
     memberchk( version(VrsIn), All ),
     to_codes( VrsIn, VrsPfx ),
     memberchk( separator(SepIn), All ),
     % to_codes( SepIn, Sep ),
     atom_codes( SepIn, Sep ),
     memberchk( extension(ExtOpt), All ),
     option_in_values_or_else_default( report, Opts, RepVls, error, Defs, Rep ),
     file_and_extension( FileIn, ExtOpt, Stem, Ext ),
     findall( add(Add,Lgt), member(add(Add,Lgt),Opts), Additions ),
     atom_codes( Stem, StemCs ),
     uf_unique_additions( Additions, Sep, StemCs, UnqStemCs ),
     atom_codes( UnqStem, UnqStemCs ),
     % has_extension( Test, UnqStem, Ext ),
     file_name_extension( UnqStem, Ext, Test ),
     ( type_exists(Test,All) ->
          append( Sep, VrsPfx, Vrs ),
          append( UnqStemCs, Vrs, VrsRnmCs ),
          append( VrsRnmCs, [0'0,0'1], FirstRnmCs ),
          atom_codes( First, FirstRnmCs ),
          uf_ex_err( Rep, Test, First ),
          unique_filename_1( First, Ext, Vrs, Rep, UnqFile, Opts )
          ;
          UnqFile = Test
     ),
     !.

unique_filename_1( Stem, Ext, Vrs, Rep, UnqFile, Opts ) :-
     % has_extension( File, Stem, Ext ),
     file_name_extension( Stem, Ext, File ),
     type_exists( File, Opts ),
     !,
     atom_codes( Stem, StemCs ),
     next_filename( StemCs, Vrs, NextStemCs ),
     atom_codes( NextStem, NextStemCs ),
     uf_ex_err( Rep, File, NextStem ),
     unique_filename_1( NextStem, Ext, Vrs, Rep, UnqFile, Opts ).
unique_filename_1( Stem, Ext, _Vrs, _Rep, File, _Opts ) :-
     file_name_extension( Stem, Ext, File ).
     % has_extension( File, Stem, Ext ).

type_exists( File, Opts ) :-
     memberchk( type(Type), Opts ),
     type_exists_1( Type, File ).

type_exists_1( file, File ) :-
     exists_file( File ).
type_exists_1( dir, Dir ) :-
     exists_directory( Dir ).

uf_ex_err( true, Exists, New ) :-
     write( user_error,
          file_name_change_due_to_existance(Exists, New) ),
     nl( user_error ).
uf_ex_err( false, _Exists, _New ).

next_filename( CharListIn, Vrs, CharListOu_T ) :-
     append( Proper, "/", CharListIn ),
     !,
     next_filename( Proper, Vrs, CharListOut1 ),
     append( CharListOut1, "/", CharListOu_T ).
next_filename( Stem, Vrs, NxtStem ) :-
     % i dont think this necessary any longer
     % ( (break_list_on_list( CharListIn, ".", Main, Vrss ),
          %    \+ member( 0'/, Vrss ) ) ->
          %    append( ".", Vrss, DotVrss )
          %    ; 
          %    ( append( Main, "/", CharListIn ) ->
          %     DotVrss = "/"
          %     ;
          %     Main = CharListIn, DotVrss = []
          %    )
     % ),
     ( break_list_on_list( Stem, Vrs, Trunk, Vers ) ->
          next_version( Vers, NextVers )
          ;   
          Trunk = Stem, NextVers = "01"
     ),
     flatten( [Trunk,Vrs,NextVers], NxtStem ).

next_version( [F,S], [F1,S1] ) :-
     next_digit( S, S1, C ),
     ( C =:= 1 ->
          next_digit( F, F1, _ )
          ;
          F1 is F
     ).

next_digit( Din, Dout, Carry ) :-
     Cd9 is 0'9,
     ( Din < Cd9 ->
             Dout is Din + 1,
             Carry is 0
             ;
             ( Din=:=Cd9 -> 
                    Dout is 0'0, 
                    Carry is 1
                    ;
                    Here = unique_filename/3,
                    atom_codes( Atm, [Din] ),
                    throw( error(type_error(digit,Atm),Here) )
               )
     ).

uf_unique_additions( [], _Sep, Stem, Stem ).
uf_unique_additions( [add(Call,Lgt)|T], Sep, Acc, Stem ) :-
     ( call( Call, Res ) ->
          to_codes( Res, ResCs ),
          length( ResCs, ResLgt ),
          ( ResLgt=<Lgt -> 
               AddCs = ResCs
               ;
               break_nth( Lgt, ResCs, AddCs, _ )
          ),
          append( Sep, AddCs, Sfx ),
          append( Acc, Sfx, Nxt )
          ;
          Nxt = Acc
     ),
     uf_unique_additions( T, Sep, Nxt, Stem ).

file_and_extension( FileIn, ExtOpt, Stem, Ext ) :-
     % ( has_extension(FileIn,StemHas,ExtHas) ->
     ( file_name_extension(StemHas,ExtHas,FileIn) ->
          ( ExtOpt==ExtHas -> 
               Stem = StemHas,
               Ext = ExtHas
               ;
               ( ExtOpt==null__ -> 
                    Stem = StemHas,
                    Ext = ExtHas
                    ;
                    Stem = FileIn,
                    Ext = ExtOpt
               )
          )
          ;
          Stem = FileIn,
          ( ExtOpt==null__ -> 
               Ext = ''
               ;
               Ext = ExtOpt
          )
     ).

to_codes( Atom, Codes ) :-
     atom( Atom ),
     !,
     atom_codes( Atom, Codes ).
to_codes( Number, Codes ) :-
     number( Number ),
     !,
     number_codes( Number, Codes ).
