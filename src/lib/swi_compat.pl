%% library(ugraph) -> library(ugraphs) in manual.
%% double check select/3 (and friends in lists, and ordered).

/*
user:bb_put( Key, New ) :-
	nb_setval( Key, New ).

user:bb_get( Key, Old ) :-
	Err=error( existence_error(variable, Key), context(system:nb_getval/2, _)),
	catch( nb_getval(Key,Old), Err, fail ).
	% copy_term( Stored, Old ).
     */

:- (   stream_property(user_output, tty(true)),
        getenv('TERM', xterm)
    ->  load_files(library(ansi_term), [silent(true)])  
    ;   true
    ).

% user:bb_put( PrvKey, New ) :-
bims_bb_put( PrvKey, New ) :-
     to_swi_nb_key_value( PrvKey, Key ),
     ( ground(New) -> Type = g; Type = ng ),
	nb_setval( Key, Type/New ).
% user:bb_get( PrvKey, Current ) :-
bims_bb_get( PrvKey, Current ) :-
     to_swi_nb_key_value( PrvKey, Key ),
     nb_current( Key, Type/NbCurrent ),
     ( Type == g ->
          Current = NbCurrent
          ;
          copy_term( NbCurrent, Current )
     ).
bims_bb_delete( PrvKey, Value ) :-
     to_swi_nb_key_value( PrvKey, Key ),
	nb_current( Key, Value ),
     nb_delete( Key ).

to_swi_nb_key_value( PrvKey, Key ) :-
     ( atom(PrvKey) ->
          Key = PrvKey
          ;
          ( number(PrvKey) ->
               atom_number( Key, PrvKey )
               ;
               term_to_atom( PrvKey, Key )
          )
     ).

/*
bb_get( Key, Old ) :-
	Err=error( existence_error(variable, Key), context(system:nb_getval/2, _)),
	catch( nb_getval(Key,Old), Err, fail ).

bb_delete( Key, Old ) :-
	Err=error( existence_error(variable, Key), context(system:nb_getval/2, _)),
	catch( nb_getval(Key,Old), Err, fail ),
	nb_delete( Key ).
*/

file_exists( File ) :- 
	exists_file( File ).

environ( Name, Value ) :-
	(var(Value) -> 
		getenv( Name, Value )
		;
		setenv( Name, Value )
	).

/* 2011 : swi has a sicstsus compatible mode now 

system( Sys ) :-
	shell( Sys ).

*/

remove_duplicates( List1, List2 ) :-
	list_to_set( List1, List2 ).

% sum_list( List, Sum ) :- sumlist( List, Sum ).

term_hash( Term, Hash ) :-
	hash_term( Term, Hash ).

variant( A, B ) :-
	A =@= B.

prolog_flag( user_error, Old, New ) :-
	!,
	stream_property( Old, alias(user_error) ),
	set_stream( New, alias(user_error) ).
	

prolog_flag( Flag, Old, New ) :-
	current_prolog_flag( Flag, Old ),
	set_prolog_flag( Flag, New ).

nth( A, B, C ) :-
	nth1( A, B, C ).

nth( N, List, Elem, Rest ) :-
     nth_tmp1( 1, N, List, Elem, Rest ).

nth_tmp1( N, N, [H|T], H, T ).
nth_tmp1( In, N, [H|T], Elem, [H|Rest] ) :-
     Nn is In + 1,
     nth_tmp1( Nn, N, T, Elem, Rest ).

datime( datime(Yr,Mo,Da,Hr,Mi,Se) ) :-
	get_time( Time ),
     stamp_date_time( Time, date(Yr, Mo, Da, Hr, Mi, Se, _, _, _ ), local ).
	% convert_time( Time, Yr, Mo, Da, Hr, Mi, Se, _ ).

% Copied from Yap sources 2006 May
/* no longer needed !. 20101201, probably for some time now.
ord_union([], []).
ord_union([Set|Sets], Union) :-
    length([Set|Sets], NumberOfSets),
    ord_union_all(NumberOfSets, [Set|Sets], Union, []).

ord_union_all(N,Sets0,Union,Sets) :-
    (  N=:=1  -> Sets0=[Union|Sets]
    ;  N=:=2  -> Sets0=[Set1,Set2|Sets], 
                 ord_union(Set1,Set2,Union)
    ;  A is N>>1,
       Z is N-A,
       ord_union_all(A, Sets0, X, Sets1),
       ord_union_all(Z, Sets1, Y, Sets),
       ord_union(X, Y, Union)
    ).
*/

:- multifile( message_hook/3 ).
:- dynamic( message_hook/3 ).

message_hook( Term, informational, _ ) :-
	(Term = ok(_); Term = exit(_,_)),
	!,
	write( user_error, '% ' ),
	write( user_error, Term ),
	nl( user_error ).

/* depreciated in SWI sometime in 2011, i believe
:- arithmetic_function(exp/2).

exp( A, B, C ) :-
	C is A ^ B.
     */

mktemp( Base, File) :-
	atom_concat( RealBase, 'XXXXXX' ,Base ),
	mktemp_1( RealBase, File ).

mktemp_1( RealBase, File ) :-
	Rgt is 0'z + 1,
	Excl = [[0'9,0'A],[0'Z,0'a]],
	randoms_in_range_with_exclusions( 6, [0'0,Rgt], Excl, Rands ),
	atom_codes( Psfx, Rands ),
	atom_concat( RealBase, Psfx, PrvFile ),
	( exists_file(PrvFile) ->
		mktemp_1( RealBase, File )
		;
		File = PrvFile
	).

randoms_in_range_with_exclusions( 0, _Range, _Excl, Rands ) :-
	!,
	Rands = [].
randoms_in_range_with_exclusions( N, Range, Excl, Rands ) :-
	Range = [From,To],
	% random( From, To, Rnd ),
	PrvRnd is random( To - From ),
	Rnd is PrvRnd + From,
	randoms_in_range_with_exclusions_1( Rnd, N, Range, Excl, Rands ).

randoms_in_range_with_exclusions_1( Rnd, N, Range, Excl, Rands ) :-
	ranges_include_number_excluding_marks( Excl, Rnd ),
	!,
	randoms_in_range_with_exclusions( N, Range, Excl, Rands ).
randoms_in_range_with_exclusions_1( Rnd, N, Range, Excl, Rands ) :-
	NxN is N - 1,
	Rands = [Rnd|TRands],
	randoms_in_range_with_exclusions( NxN, Range, Excl, TRands ).

ranges_include_number_excluding_marks( [[Min,Max]|_T], Num ) :-
	Num > Min, Num < Max,
	!.
ranges_include_number_excluding_marks( [_H|T], Num ) :-
	ranges_include_number_excluding_marks( T, Num ).

% that is based on quick test on Yap. 
% Currently i dont have access to sicstus.
/*
ord_member( Elem, Set ) :-
	ord_memberchk( Elem, Set ).
     */

at_end_of_line( Stream ) :-
	peek_byte( Stream, 10 ).

host_name( Host ) :-
     environ( 'HOST', Host ).
