:- dynamic( portray/1 ).

% :- set_prolog_flag( verbose_load, true ).

:- ensure_loaded('../src/init_lib').

% :- ensure_loaded( library(on_pl) ).        % pl/2.
                                             % library(pl) is not accepted in Yap!?
:- ensure_loaded(library(lists)).     % is_list/1.
:- ensure_loaded(library(random)).

:- pl( yap(_Y), set_prolog_flag(update_semantics,logical) ).
:- pl( yap(_Y), set_prolog_flag(unknown,error) ).
:- pl( yap(_Y), set_prolog_flag(single_var_warnings,on) ).

:- ensure_loaded(all_dynamic).
:- ensure_loaded(ad_to_slp).
:- ensure_loaded(kernel).
:- ensure_loaded(version).
% :- ensure_loaded(model_type_calls).

:- lib(stoics_lib:en_list/2).
:- lib(lss/0).
:- lib(dbg/2).                    %
:- lib(werr/2 ).                   % /1.
:- lib(mcmcms_top_dir/1).          % bb_puts it.
:- lib(is_list_of_n_vars/2).
:- lib(unique_filename/3).
:- lib(is_letter/1).
:- lib(assert_unique/1).
:- lib(open_mode/3).
:- lib(call_error/3).
:- lib(bb_default/2).
:- lib(delete_file_if/1).
% fixme: copy_stream_data/2 is built-in in SWI, see copy_stream_on_stream.pl if you need compatibility in the future

% :- pl( sicstus(_S), ensure_loaded( library(number_atom) ) ).        % /2.

/** mcmcms( F, S, O, K, M, B, P, R, D, H, Seed, SGl ).

Main enty point for a single chain.
Args:
    * F: File for runs terms output: fixme ... fixing
    * S: Stats ouput file
    * O: Prints output file (new in bims)
    * K: Backtracking id (fixme: give details here)
    * M: model space
    * B: The prior ? (fixme) why B
    * P: Backtracking parameter I think... (fixme)
    * R: Number of iterations (chain length as of bims).
    * D: data Goal
    * H: Hot chains, leave [] for now. fixme
    * S: seed 1 - 1000   % S1-3: random seeds 
    * Goal: Top goal

*/
mcmcms( F,S,O,K,_M,B,P,R,D,H,Seed,SGl ) :-
     Np = P,
     maplist( open_mode(write), [F,S], [ResOut,SttOut] ),
     mcmcms_open_report_file( O, RepOut ),
     % print_args( Stats, F,S,K,M,B,P,R,D,H,Seed ),
     load_data_and_put_dgoal( D ),
     bims_random_index_set_seeds( Seed, SttOut ),
     bb_default( msd, rm ),
     bims_bb_put( bk_failures, [] ),
     trace,
     mcmcms_load_prior( B, TmpLoad ),
     mcmcms_main( SttOut, ResOut, RepOut, S, K, SGl, R, Np, H, TmpLoad ),
     maplist( close, [ResOut,SttOut] ).

mcmcms_main( SttOut, ResOut, RepOut, S, K, SGl, R, Np, H, TmpLoad ) :-
     main_out( SttOut, ResOut, RepOut, S, K, SGl, R, Np, H, TmpLoad ),
     !.
mcmcms_main( _SttOut, _ResOut, _RepOut, _S, _K, _SGl, _R, _Np, _H, TmpLoad ) :-
     mcmcms_maintainer( Maintainer ),
     Failure = [['Main MCMC call ended with failure.'],
                ['Check your DLP, the options of bims/1 and read the manual.'],
                ['You can use option debug(true) to view check points of the program execution.'],
                ['If you cannot resolve the problem don\'t hasitate to send a bug report, to:'],
                [Maintainer]],
     delete_file_if( TmpLoad ),
     werr( Failure ),
     abort.

main_out( SttO, ResO, RepO, S, Kid, SGl, R, Np, H ) :-
     main_out( SttO, ResO, RepO, S, Kid, SGl, R, Np, H, null__ ).

main_out( SttO, ResO, RepO, S, Kid, SGl, R, Np, H, TmpLoad ) :-
     % for the following, use pl/1 instead/additionally.
     ( TmpLoad == null__ ->
          true
          ;
          open( TmpLoad, read, ReadFrom  ),
          ( atom_concat(StatsStem,'.stats',S) ->
               atom_concat( StatsStem, '.dlp', SlpF ),
               open( SlpF, write, SlpO )
               ;
               % fixme: check this:
               % Stats = SlpO
               SttO = SlpO
          ),
          copy_stream_data( ReadFrom, SlpO ),
          close( ReadFrom ),
          current_output( OldCOut ),
          werr( ['--- start listing of last_skips/2'], info, '', SlpO ),
          set_output( SlpO),
          listing( last_skips/2 ),
          set_output( OldCOut ),
          werr( ['--- end   listing of last_skips/2'], info, '', SlpO ),
          ( var(SlpF) -> true; close(SlpO) ),
          delete_file_if( TmpLoad )
     ),
     main_statistics( StartAt, SttO ),
     %% bid_exclude_lbl( Kid ),
     assert_unique( bims_report:stream(RepO) ),
     kernel( SGl, H, R, Kid, Np, ResO ),
     main_statistics( EndWith, SttO),
     Total is (EndWith - StartAt) / 1000,
     nl( SttO ),
     pc( SttO, total(((EndWith - StartAt) / 1000), Total) ),
     bims_bb_get( bk_failures, Failures ),
     ( Failures = [] -> 
          % just be silent...
          true
          ;
          write( SttO, 'Backtracking failed on the following iterations: ' ),
          write( SttO, Failures )
     ),
     nl( SttO ).

%% mcmcms_load_prior( +Dlp, ?Tmp )
% 
% Transform Dlp into its equivalent logic program that can be used 
% for MCMC sampling. Tmp = null__ when Dlp == null__ and nothing is transformed.
% If Tmp is ground the file is used as is, else a filename of the form
% _translated__slp$HOSTNAME_ is created.
%
mcmcms_load_prior( DlpF, PlF ) :-
     DlpF == null__,
     !,
     PlF == null__.
mcmcms_load_prior( DlpF, PlF ) :-
     mcmcms_load_prior_pl_file( PlF ),
     all_dynamic( DlpF ),
     ad_to_slp( [rm(false),tmp(PlF)] ).

mcmcms_load_prior_pl_file( PlF ) :-
     var(PlF),
     !,
     UFOpts = [report(false),add(environ('HOSTNAME'),8)],
     unique_filename( translated__slp, PlF, UFOpts ).


% print_args( Stats, F,S,K,M,B,P,R,D,H,S1,S2,S3 ) :-
print_args( Stats, F,S,K,M,B,P,R,D,H,Seed ) :-
     mcmcms_version( MsVer ),
     host_name( Host ),
     prolog_flag( version, ThisPlEngine ),
     pc( Stats, on(ThisPlEngine) ),
     (bims_bb_get(last_data, ld(_TheData,DataFile)) ->
          % a hack communication with run.pl
          true
          ;
          DataFile = D % and hope for the best
     ),
     pc( Stats, [mcmcms(MsVer),host(Host)] ),
     nl( Stats ),
     % functor( D, NameD, ArD ),
     pc( Stats, [model_type(M),results_file(F),stats_file(S),
            kernel(K),prior_file(B), backtrack_probability(P),
            repeats(R),data_call(D), data_file(DataFile), hot_chains(H),
            seed(Seed)
            % seeds(S1,S2,S3)
            ] ),
     nl( Stats ).
           
main_statistics( Sum, Stats ) :-
     datime( Datime ), 
     nl( Stats ),
     pc( Stats, Datime ),
     stream_property( Old, alias(user_error) ),
     set_stream( Stats, alias(user_error) ),
     % prolog_flag( user_error, Old, Stats ),
     pl( Pl ),
     ( Pl = sicstus(_) ->
          statistics( atom_garbage_collection, [_Nofag,_Ab,OvAt] )
          ;
          ( Pl = yap(_) ->
               statistics( cputime, [_OvCt,CpuTime] ),
               pc( Stats, yap_cputime(CpuTime) ),
               OvAt is 0
               ; % else it is swi
               statistics( cputime, CpuTime ),
               pc( user_error, swi_cputime(CpuTime) ),
               OvAt is 0
          )
     ),
     statistics( runtime, [OvRt,_RlRt] ),
     statistics( garbage_collection, [_Nofgc,_Bfrd,OvGt] ),
     ( statistics( stack_shifts, [_Nofls,_Nofts,OvSt] ) ->
          true; OvSt is 0 ),
     Sum is (OvRt + OvGt + OvSt + OvAt),
     pl( swi(_), 
          ( current_output(SwiOut),
            set_output(Stats),
            nl, write( '/*' ), nl,
            statistics,
            nl, write( '*/' ), nl,
            set_output(SwiOut)
          )
          ,
          ( nl(Stats), write(Stats,'/*'),nl(Stats),
            statistics, write(Stats, '*/' ),nl(Stats))
     ),
     % prolog_flag( user_error, _St, Old ).
     set_stream( Old, alias(user_error) ).



load_data_and_put_dgoal( D ) :-
     goal_spec( D, SpecD ),
     ( current_predicate(user:SpecD) -> 
          ( call( user:D ) ->
               true
               ;
               werr( [['Warning: data call failure, for: ', D ]] )
          )
          ;
          ( D == null ->
               true
               ;
               werr( [['Warning: data call does not exists, for: ', D ]] )
          )
     ),
     % findall( DA, current_predicate(data/DA), DAs ),
     % Change 2.
     ( current_predicate(data/2) ->
          ( (data(Instance,_InstTms),is_list(Instance)) -> 
               length( Instance, InstLength ),
               is_list_of_n_vars( InstLength, VarListOfLengthVs ),
               bims_bb_put( multi_data_format, data(VarListOfLengthVs,_TmsVar) )
               ;
               werr( [['Cannot locate data/2. Aborting...']] ), abort
          )
          ;
          true
     ).
     /* 
     ( DAs = [OkDA] -> 
          ( OkDA = 
          functor( Datum, data, OkDA ),
          bims_bb_put( data_format, Datum ),
          MultiDA is OkDA + 1,
          functor( MultiDatum, data, MultiDA ),
          bims_bb_put( multi_data_format, MultiDA-MultiDatum )
          ;
          werr( [['There should be only one data/n predicate defined.'],
                 ['Aborting, after finding multiple definitions: ',DAs]] ),
                 abort
     ).
     */

pc( Stream, Clause ) :-
     en_list( Clause, Clauses ),
     pc_1( Clauses, Stream ).

pc_1( [], _Stream ).
pc_1( [H|T], Stream ) :-
     % portray_clause( Stream, H ),
     mcmcms_write_fact( Stream, H ),
     pc_1( T, Stream ).

bims_random_index_set_seeds( Seed, SttOut ) :-
     integer( Seed ),
     0 < Seed , Seed =< 1000,
     absolute_file_name( bims(src/lib/'random_index_seeds.pl'), Ris ),
     open( Ris, read, In ),
     read_term( In, InTerm, [] ),
     bims_nth_random_term( Seed, InTerm, In, Rand ),
     close( In ),
     debug( bims, 'Seed: ~d (-> ~w)', [Seed,Rand] ),
     setrand( Rand ),
     mcmcms_write_fact( SttOut, seed(Seed) ),
     % portray_clause( SttOut, seed(Seed) ),
     mcmcms_write_fact( SttOut, rand(Rand) ).
     % portray_clause( SttOut, rand(Rand) ).

bims_nth_random_term( 1, Term, _In, Rand ) :- !,
     arg( 2, Term, Rand ).
bims_nth_random_term( I, _Term, In, Rand ) :-
     read_term( In, New, [] ),
     H is I - 1,
     bims_nth_random_term( H, New, In, Rand ).

%% mcmcms_open_report_file( File, Stream ).
%
% Only open File for writing on Stream if there are things
% current runs are reporting on.
%
mcmcms_open_report_file( File, Report ) :-
     findall( This/Arity, ( bims_report:report(This),
                      known_reportable_term(This,Arity,_)
                    ), 
                         These ),
     mcmcms_tokens_open_report_file( These, File, Report ).

mcmcms_tokens_open_report_file( [], _File, null ).
mcmcms_tokens_open_report_file( [H|T], File, Stream ) :-
     open( File, write, Stream ),
     mcmcms_tokens_discontineous( T, H, Stream ).

mcmcms_tokens_discontineous( [], _H, _Stream ).
mcmcms_tokens_discontineous( [B|T], A, Stream ) :-
     maplist( mcmcms_token_discontiguous(Stream), [A,B|T] ).

mcmcms_token_discontiguous( Stream, PredId ) :-
     write( Stream, ':- ' ),
     mcmcms_write_fact( Stream, discontiguous(PredId) ).

mcmcms_write_fact( Stream, Term ) :-
     % write_term( Stream, Term, [quoted(true),ignore_ops(true)] ),
     % write_term( Stream, Term, [quoted(true),portrayed(true)] ),
     % write( Stream, '.' ), nl( Stream ).
     current_prolog_flag( allow_dot_in_atom, Allow ),
     set_prolog_flag( allow_dot_in_atom, false ),
     portray_clause( Stream, Term ),
     set_prolog_flag( allow_dot_in_atom, Allow ).


% user:portray( A.B ) :- write( a(A)-b(B) ), nl, fail.
% user:portray( C ) :- write( c(C) ), nl, fail.
