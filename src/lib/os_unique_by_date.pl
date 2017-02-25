
:- use_module( library(date) ).
:- ensure_loaded( library(apply) ).
:- requires( get_date_time/1 ).
:- requires( break_nth/4 ).
:- requires( en_list/2 ).

os_unique_by_date_test :-
	use_module( library(socket) ),  
     gethostname( Host ),
     ( os_unique_by_date( [res,Host], Bname, [] )
	  ; os_unique_by_date( [res,Host], Bname, [min_length(da,3)] )
	  ; os_unique_by_date( [res,Host], Bname, [max_length(ya,3)] )
	),
     write( b_name(Bname) ), nl.

/*
os_unique_by_date_test( two ) :-
     gethostname( Host ), % SWI specific? check Yap
     write( b_name(Bname) ), nl.
	*/

%% os_unique_by_date( +Token, -OsEntry ).
%% os_unique_by_date( +Token, -OsEntry, +Opts ).
%  
% Create a uniquely named file name or directory named OsEntry by using date elements
% and a Token. Token can be a list in which case the Separator option will
% also apply within the token parts.
%  
% The predicate does not only provide the name of OsEntry, it also creates it. 
%
% Opts: 
%  * by([ye,mo,da,[ho,mi],[se]]) 
%    how to group date parts in getting a unique entry. The default does
%    YeMoDa first, then adds HoMi and on the thrid attempt adds Seconds.
%  * date_sep(Dsep='.')     
%    inter date component separator
%  * ext(Ext=csv)           
%    extension to add to OsEntry iff type is file
%  * max_length(Dp=ye,Ye=2) 
%    max length of date Id (_ye_,_mo_,_da_), and an integer
%  * min_length(Id='',Len)
%    min length of date Id (_ye_,_mo_,_da_), and an integer
%  * place_token(Plc=before)
%    or _after_, where to place the token in relation to the date
%  * token_sep(Sep='-')
%    separator to be used in bonding Token and Date
%  * type(Type=dir)
%    should the unique entry be a _dir_ectory or a _file_.
% 
% Id and DP can be a free variables in which case they match everything.
%
% If you call this twice within a single second there is all the chance it might fail.
%
%==
% ?- os_unique_by_date( res, Dname ).
% Dname = 'res-14.05.22'.
% 
% ?- os_unique_by_date( res, Dname, [] ).
% Dname = 'res-14.05.22.10.40'.
% 
% ?- os_unique_by_date( res, Dname, [] ).
% Dname = 'res-14.05.22.10.40.46'.
% 
% ?- os_unique_by_date( res, Dname, [] ).
% Dname = 'res-14.05.22.10.40.57'.
% 
% ?- os_unique_by_date( res, Dname, [min_length(_,3)] ).
% Dname = 'res-014.005.022'.
%
% ?- os_unique_by_date( res, Dname, [token_sep('+'),date_sep(':'),place_token(after),type(file)] ).
% Fname = '14:05:22+res.csv'.
% 
% ?- os_unique_by_date( res, Dname, [token_sep('+'),date_sep(':'),place_token(after),type(file)] ).
% Dname = '14:05:22:11:03+res.csv'.
% 
% ==
%
% Used to be unique_entry_by_date/n.
%
% @author nicos angelopoulos
% @version  0.2 2014/5/22
%
os_unique_by_date( Tkn, Bname ) :-
    os_unique_by_date( Tkn, Bname, [] ).
os_unique_by_date( Tkn, Bname, ArgS ) :-
    en_list( ArgS, Args ),
    % options_append( os_unique_by_date, InOpts, Opts ),
    % os_unique_by_date_defaults( Defs ) :- 
    Defs = [ date_sep('.'),token_sep('-'),by([ye,mo,da,[ho,mi],[se]]),place_token(before),
              type(dir), max_length(ya,2), min_length(_,2),ext(csv) ],
    append( Args, Defs, Opts ),
    memberchk( by(By), Opts ),
    construct_unique_base_name_by_date( By, Tkn, Opts, Bname ).

construct_unique_base_name_by_date( By, Tkn, All, Bname ) :-
     partition( atom, By, ByAtoms, ByLists ),
     get_date_time( Datime ),
     findall( max(Key,Len), (member(max_length(KeyIn,Len),All),expand_date_key(KeyIn,Datime,Key)), XLengths ),
     findall( min(Key,Len), (member(min_length(KeyIn,Len),All),expand_date_key(KeyIn,Datime,Key)), NLengths ),
     append( XLengths, NLengths, Lengths ),
     findall_date_components( ByAtoms, Lengths, Dcomps ),
     memberchk( type(Type), All ),
     memberchk( ext(Ext), All ),
	memberchk( date_sep(DSep), All ),
     atomic_list_concat( Dcomps, DSep, DateBit ),
	en_list( Tkn, Tkns ), 
	memberchk( token_sep(TSep), All ),
	atomic_list_concat( Tkns, TSep, TknConc ),
	memberchk( place_token(PlcTkn), All ),
	place_token_sep_date_concat( PlcTkn, TknConc, TSep, DateBit, CurrStem ),
	type_ext_full( Type, Ext, CurrStem, Current ),
     keep_to_unique_base_name_by_date( Current, ByLists, Lengths, PlcTkn, TSep, DSep, Type, Ext, Bname ),
     ( Type == file ->
          open( Bname, write, Out ), close( Out )
          ;
          Type == dir,
          make_directory(Bname)
     ).

type_ext_full( dir, _Ext, Current, Current ).
type_ext_full( file, Ext, Current, Full ) :-
	file_name_extension( Current, Ext, Full ).

findall_date_components( ByAtoms, Lens, Dcomps ) :-
     get_date_time( Datime ),
     findall(  Dcomp, ( member(ByA,ByAtoms),
                        date_time_value(Key,Datime,Val),
                        number( Val ),
                        sub_atom(Key,0,_,_,ByA),
                        IntVal is integer(Val),
                        procruste( Key, IntVal, Lens, Dcomp )
                      ), 
                         Dcomps ).

place_token_sep_date_concat( before, Tkn, Sep, Date, Conc ) :-
	atomic_list_concat( [Tkn,Date], Sep, Conc ).
place_token_sep_date_concat( after, Tkn, Sep, Date, Conc ) :-
	atomic_list_concat( [Date,Tkn], Sep, Conc ).

keep_to_unique_base_name_by_date( Current, _ByLists, _Lengths, _Plc, _TSep, _DSep, _Type, _Ext, Bname ) :-
	% fixme:
     \+ exists_file( Current ),
     \+ exists_directory( Current ),
     !,
     Bname = Current.
keep_to_unique_base_name_by_date( Full, [H|T], Lengths, Plc, TSep, DSep, Type, Ext, Bname ) :-
     findall_date_components( H, Lengths, Dcomps ),
	type_ext_full( Type, Ext, Current, Full ),
	place_stem_date_fragment( Plc, Current, TSep, DSep, Dcomps, Next ),
	type_ext_full( Type, Ext, Next, NextFull ),
     % atomic_list_concat( [Current|Dcomps], IdSep, Next ),
     % we could introduce another separator here.
     keep_to_unique_base_name_by_date( NextFull, T, Lengths, Plc, TSep, DSep, Type, Ext, Bname ).

place_stem_date_fragment( before, SoFar, _TSep, DSep, Dcomps, Next ) :-
     atomic_list_concat( [SoFar|Dcomps], DSep, Next ).
place_stem_date_fragment( after, SoFar, TSep, DSep, Dcomps, Next ) :-
	atomic_list_concat( [Dsofar|Rem], TSep, SoFar ),
     atomic_list_concat( [Dsofar|Dcomps], DSep, Dnext ),
	atomic_list_concat( [Dnext|Rem], TSep, Next ).

procruste( Key, Int, Lens, Dcomp ) :-
     number_codes( Int, IntCs ),
     length( IntCs, IntLen ),
     copy_term( Lens, CopyLens ),
     ( memberchk(max(Key,Max),CopyLens) -> 
          ( Max<IntLen ->
              Marg is IntLen - Max,
              break_nth( Marg, IntCs, _, RCs )
              ;
              RCs = IntCs 
          )
          ;
          RCs = IntCs 
     ),
     length( RCs, Rlen ),
     ( memberchk(min(Key,Min),CopyLens) ->
          (Min>Rlen ->
               Fill is Min - Rlen,
               findall( 0'0, between(1,Fill,_), ZCs )
               ;
               ZCs = []
          )
          ;
          ZCs = []
     ),
     append( ZCs, RCs, AllCs ),
     atom_codes( Dcomp, AllCs ).

expand_date_key( _KeyIn, _Datime, Key ) :-
     var(Key),
     !.
expand_date_key( KeyIn, Datime, Key ) :-
     date_time_value( Key, Datime, _Value ),
     sub_atom( Key, 0, _, _, KeyIn ),
     !.
