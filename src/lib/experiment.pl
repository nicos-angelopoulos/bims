% THis SHOULDNOT be used. i have to remove all refs in the calling
% code and zap it...

% :- prolog_load_context( directory, Dir ), assert( file_search_path(exp_dir,Dir) ).
:- pl( sicstus(_A), (
		prolog_load_context( directory, Path ), 
		retractall( file_search_path(exp_dir,_OldPath) ),
		assert( file_search_path(exp_dir,Path)) 
	) ).
:- pl( yap(_A), (
		getcwd( Path ),
		assert( file_search_path(exp_dir,Path) )
	) ).

:- pl( yap(_A), use_module(library('yap/file_exists')) ).
:- pl( yap(_A), use_module(library('yap/host_name')) ).
:- pl( yap(_A), use_module(library('yap/datime')) ).
:- pl( yap(_A), use_module(library('yap/sum_list')) ).
:- pl( yap(_A), use_module(library('yap/std_afn')) ).
:- pl( sicstus(_A), ensure_loaded( 	library( system ) )).
:- ensure_loaded( 	library( lists ) ).
:- ensure_loaded( 	library( random ) ).
:- ensure_loaded( 	rnd_seeds ).

:- pl( swi(_), 
          true,
          use_module(library(terms), [variant/2]) ).

:- lib(stoics_lib:list_frequency/2).

exper_setrand(  Rand ) :-
	( var(Rand) ->
		grab_random_seeds( R1, R2, R3 ),
		Rand = rand(R1,R2,R3)
		;
		true
	),
	( (pl(sicstus(_));pl(yap(_))) ->
		setrand( Rand )
		;
		true
	).

experiment( Options, AllSolutions ) :-
	( memberchk( rand(R1,R2,R3), Options ) ->
		true
		;
		grab_random_seeds( R1, R2, R3 )
	),
	which_prolog_stats( Prolog, Stats ),
	experiment_defaults( Defaults ),
	select_option( clause(Clause,PrintAbles), Options, Defaults ),
	grab_file_and_operation( Options, Defaults, FileAtom, Op ),
	select_option( show_file_sel(FlSlBoo), Options, Defaults ),
	( FlSlBoo == yes -> 
		write_message( ['File selected for output : ', FileAtom] )
		;
		true
	),
	change_output_to_file( FileAtom, Op, Out, CurOut ),
	host_name( Host ), write_w_header( 'on Machine', write(Host) ),
	write_w_header( 'under Prolog', write(Prolog) ),
	write_w_header( 'Start time of experiment', write_datime ),
	( R1 == none ->
		true
		;
		exper_setrand( rand(R1,R2,R3) )
	),
	write_w_header( 'Random seeds', write(rand(R1,R2,R3)) ),
	( select_option( showlisting(InfoRules), Options, Defaults ) ->
		write_w_header( 'Rule base includes', write_all_matching_rule(InfoRules)), nl
		;
		true
	),
	select_option( times(Times), Options, Defaults ),
	write_w_header( 'asked to perform', write_message(
		[Times,'repetitions, of',Clause,'- printing each time :',PrintAbles, ' ']) ),
	write_w_header( 'System statistics before execution', write_many_statistics(Prolog,Stats) ),
	write_w_header( 'Output from tested clauses', true ),
	select_option( count_failures(Fails), Options, Defaults ),
	run_n_times( Times, Clause, PrintAbles, Fails, AllSolutions, AllPrinted ), nl,
	select_option( observables(Obs), Options, Defaults ),
	( var(Obs) -> Obs=AllPrinted ; true ),
	select_option( show_sol_freq(SFyn), Options, Defaults ),
	( SFyn==yes -> list_frequency(AllSolutions,FreqOfAll),
		write_w_header( 'Aggregate results', write(FreqOfAll) ), nl
		;
		true
	),
	write_w_header( 'System statistics after execution', write_many_statistics(Prolog, Stats) ),
	write_w_header( 'End time of experiment', write_datime ),
	% close( OutputStream ),
	% tell( CurOut ),
	safe_close( FileAtom, Out ),
	set_output( CurOut ),
	select_option( send_email_to(Whom), Options, Defaults ),
	send_end_mail( Whom ).

which_prolog_stats( Prolog, Stats ) :-
	pl( Prolog ),
	( Prolog = sicstus(_) ->
		Stats = [runtime,walltime]
		;
		( Prolog = eclipse(_) -> 
			Stats = [runtime,times]
			;
			( Prolog = swi(_) ->
				Stats = [cputime,inferences]
				;
				( Prolog = yap(_) ->
					Stats = [cputime,runtime]
					;
					Stats = [runtime] % well, whatever
				)
			)
		)
	).

experiment_defaults( 
	[clause(sample(s(A),P,R),A-P-R),
	 % showlisting([rule/2]),
	 show_file_sel(no),
	 times(100),
	 file(current_output),filemode(append),
	 observables(no),
	 show_sol_freq(yes),
	 count_failures(yes),
	 send_email_to(no_email)] ).

% Options :
% show_file_sel(Bool)    -> Bool==yes -> message of file used apear on
		% user_output
%  send_email_to( Whom ) -> user is the user running the process;
		% Whom = no_email for no email

% grab_random_seeds( reduced version ).
grab_random_seeds( R1, R2, R3 ) :-
	( pl( sicstus(_)) ->
		% absolute_file_name( pccp('random_pool_lock'), LockFile ),
		% absolute_file_name( pccp('random_pool'), PoolFile ),
		absolute_file_name( exp_dir('random_pool_lock'), LockFile ),
		absolute_file_name( exp_dir('random_pool'), PoolFile ),
		busy_wait_on( LockFile ),
		pop_first_three_numbers_from_file( PoolFile, R1, R2, R3 ),
		delete_file( LockFile )
		;
		( pl(yap(_)) -> 
			std_afn( exp_dir('random_pool_lock'), LockFile ),
			std_afn( exp_dir('random_pool'), PoolFile ),
			busy_wait_on( LockFile ),
			pop_first_three_numbers_from_file( PoolFile, R1, R2, R3 ),
			delete_file( LockFile )
			;
			true
		)
	).

grab_file_and_operation( Options, Defaults, FileAtom, Op ) :-
	select_option( file(OriginalFileAtom), Options, Defaults ),
	select_option( filemode(OrigOp), Options, Defaults ),
	proper_operation_n_unique_filename( OrigOp, Op, OriginalFileAtom, FileAtom ).

run_n_times( 0, _Clause, _PrintThese, _FC, [], [] ) :-
	!.
run_n_times( Times, Clause, PrintThese, Fcount, OutClauses, Observes ) :-
	% garbage_collect, % eclipse on my machine gets funny over this....so
	flush_output, 
        copy_term( [Clause,PrintThese], [TClause,TPrints] ),
        % prove( TClause ),
     ( call( TClause ) ->
		LessByOne is Times - 1,
		OutClauses = [TClause|MoreClauses],
		Observes   = [TPrints|MorePrints]
		;
		( Fcount == no ->
			LessByOne is Times,
			OutClauses = MoreClauses,
			Observes   = MorePrints
			;
			LessByOne is Times - 1,
			OutClauses = [TClause|MoreClauses],
			Observes   = [TPrints|MorePrints]
		)
	),
     !, % this is unesseccary
	write_one_experiment_s_results( Times, TClause, TPrints ),
	% this should be ramified Dec 2000
	run_n_times( LessByOne, Clause, PrintThese, Fcount, MoreClauses, MorePrints ).
	
busy_wait_on( File ) :-
	file_exists( File ),	
	write_message( ['File exists,',File] ),
	write_message( ['Now waiting until it is deleted'] ),
	waste( 10000 ),
	!,
	busy_wait_on_1( File ).
busy_wait_on( File ) :-
	open( File, write, Stream ),
	write( Stream, wait ),
	close( Stream ).

busy_wait_on_1( File ) :-
	file_exists( File ),
	waste( 10000 ),
	!,
	busy_wait_on_1( File ).
busy_wait_on_1( File ) :-
	write_message( ['File deleted',File, 'now proceeding'] ),
	busy_wait_on( File ).

waste( 0 ) :- !.
waste( N ) :-
	N1 is N  - 1,
	waste( N1 ).
	
send_end_mail( Whom ) :-
	( Whom = user ->
		environ( 'LOGNAME', User ),
		send_end_mail_to( User )
		;
		( Whom = no_email -> 
			true
			;
			send_end_mail_to( Whom )
		)
	).

send_end_mail_to( User ) :-
	host_name( Host ), 
	name( 'mailx -s ', MailPrefix ),
	% name( '/usr/ucb/mail -s ', MailPrefix ), 
		% you might need something like this instead
	name( Host, HostList ),
	append( [39|HostList], [39,32], QuotedHL ),
	name( User, UserCs ),
	( pl(eclipse(_) ) ->
		pccp_file_expansion( exp_dir('../auxil/experiment_finished'), FinMess )
		;
		( pl(yap(_)) ->
			std_afn( exp_dir('../auxil/experiment_finished'), FinMess )
			;
			absolute_file_name( exp_dir('../auxil/experiment_finished'), FinMess )
		)
	),
	name( FinMess, FinMessCs ),
	append( UserCs, [0' ,0'<, 0' |FinMessCs], MailPostFix ),
	append( MailPrefix, QuotedHL, FirstPart ),
	append( FirstPart, MailPostFix, AllList ),
	name( All, AllList ),
	shell( All ).
	% system( All ).

proper_operation_n_unique_filename( append, append, FileIn, FileIn ) :-
	!.
proper_operation_n_unique_filename( write, write, FileIn, FileOut ) :-
	!,
	make_sure_of_unique_filename( FileIn, FileOut ).
proper_operation_n_unique_filename( Operation, append, FileIn, FileIn ):-
	write_error( warning, ['One of',[write,read],'was expected, instead of, ', Operation, nl, 'Append was assumed'] ).
make_sure_of_unique_filename( user_output, user_output ) :-
	!.
make_sure_of_unique_filename( FileAtom, UniqueFile ) :-
	(file_exists( FileAtom ) -> 
			name( FileAtom, FileString ),
			get_next_filename( FileString, NextFileString ),
			name( NextFileAtom, NextFileString ),
			write_error( warning, ['File,',FileAtom,'exists, changing to',NextFileAtom] ),
			make_sure_of_unique_filename( NextFileAtom, UniqueFile )
				  ;
			UniqueFile = FileAtom
	).

get_next_filename( CharListIn, CharListOu_T ) :-
	last( CharListIn, LastChar ),
	append( FirstPart, [LastChar], CharListIn ),
	NewLastChar is LastChar + 1,
	append( FirstPart, [NewLastChar], CharListOu_T ).
	
%%%%%%%%   End of Meta-Meta Trials Predicates
select_option( Element, List1, _List2 ) :-
	lists:member( Element, List1 ),
	!.
select_option( Element, _List1, List2 ) :-
	lists:member( Element, List2 ).

change_output_to_file( current_output, _Operation, CurOut, CurOut ) :-
	!,
	write( first_clause ),
	current_output( CurOut ).
change_output_to_file( FileAtom, Operation, FileStream, CurOut ) :-
	current_output( CurOut ),
	open( FileAtom, Operation, FileStream ),
	set_output( FileStream ).
	% tell( FileStream ).

% safe_close( user, Stream ) :- !,
	% set_output( Stream ).
% safe_close( Other ) :- 
	% close( Other ),
	% set_output( Stream ).
safe_close( current_output, user ) :- !.
safe_close( _Other, Stream ) :- 
	close( Stream ).

% write predicates

write_w_header( Header, Goal ) :-
	write( '----- ' ), write( Header ),
	write( ' : ' ), nl, tab( 2 ),
	call( Goal ), nl.

write_all_matching_rule( Rule ) :-
	\+ is_list( Rule ),
	!,
	listing( Rule ).
write_all_matching_rule( [Rule|Rules] ) :-
	listing( Rule ),
	write_all_matching_rule( Rules ).
write_all_matching_rule( [] ).

write_many_statistics( Prolog, [H|T] ) :-
	write_one_statistic( Prolog, H ),
	write_many_statistics( Prolog, T ).
write_many_statistics( _Prolog, [] ).

write_one_statistic( Prolog, StatKey ) :-
	statistics( StatKey, StatVal ), 
	write( Prolog ),
	write( ' statistic, for ' ),
	write( StatKey ),
	write( ' is ' ),
	write( StatVal ),
	write( '.' ), nl.

write_one_experiment_s_results( Tim, Clause, Print ) :-
	six_space_numb( Tim ),
		% format/2 ensures a pretty print effect 
		% for the experiment number, replace with
		% write( Tim ),
		% if there is a compatibility problem.
	write( ' : ' ),
	write( Clause ), write( ' : ' ),
	write( Print ), nl.

six_space_numb( Tim ) :-
	pl( eclipse(_) ),
	!,
	printf( "%t%6d|", [Tim] ).
six_space_numb( Tim ) :-
	format( "~t~d~6|", [Tim] ).

write_error( warning, Message ) :-
	!,
	write_message( Message ).
write_error( fatal, Message ) :-
	write_message( Message ),
	write_message( ['Execution aborted'] ),
	fail.

write_message( [Var|More] ) :-
        var( Var ),
        !,
        write( Var ),
        write_message( More ).
write_message( [nl|Tail] ) :-
	!, nl, write_message( Tail ).
write_message( [F,S|Tail] ) :-
	!,
	write( F ), write( ' ' ),
	write_message( [S|Tail] ).
write_message( [S] ) :-
	!,
	write( S ), write( '.' ), nl.
write_message( AnyOther ) :-
        write( AnyOther ), nl.

write_datime :-
	pl( eclipse(_) ),
	!,
	date( Date ), write( Date ).

write_datime :-
	datime( datime(Year,Month,Day,Hour,Mins,Sec) ),
	three_letter_months( SymbolicMonths ),
	nth( Month, SymbolicMonths, SymbolicMonth ),
	day_trailer( Day, Trailer ),
	write( ' At ' ),
	write( Hour ), write( ':' ),
	write( Mins ), write( ':' ), 
	write( Sec ), write( ' on ' ),
	write( Day ), write( Trailer ),
	write( ' of ' ), write( SymbolicMonth ),
	write( ', ' ), write( Year ),
	write( '.' ), nl.


three_letter_months( ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'] ).

day_trailer( 1, 'st' ) :- !.
day_trailer( 2, 'nd' ) :- !.
day_trailer( 3, 'rd' ) :- !.
day_trailer( _AllOther, 'th' ).
