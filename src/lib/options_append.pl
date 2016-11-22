
% :- use_module( library(debug_call) ).
:- requires( debugging_status/2 ).
:- requires( en_list/2 ).

:- requires( message_report/3 ).
:- requires( select_all/4 ).
:- requires( term_template/2 ).
:- requires( name_term/2 ).
:- requires( select_first/3 ).

options_append_known_process_option( debug ).

%% options_append( +PredName, +OptS, -All ).
%% options_append( +PredName, +OptS, -All, +OAopts ).
%
% Look for PredName_defaults/1 and if that exists append it to OptS to get All.
% OptS is casted to a list before the append, so single terms are allowed as options.
% Listens to debug(options_append).
% 
% The predicate can process debug(Dbg) a commonly used option. Default should be provided by PredName caller.
% The infrastructure allows for other options to be added easily.
% 
% OAopts term or list of: 
%
%  * extra_arg(Arg)          
%    multiple allowed. All Arg(s) so passed are added to the Args passed to the defaults predicate, 
%    but not to the generated options. Allows for instance to pass argments of the call itself 
%    to the defaults predicate without those arguments being added to the Options list
%
%  * foreign(Foreign)
%    instantiates to all options that do not have matching default term structure
%
%  * debug(Dbg=false)   
%    if _true_ debug this call, and call prolog_debug:debug_topic(Pname). 
%    Else Dbg can be a debug term or list of debug terms.
% 
%  * funnel(Proccess)    
%    as process() below, but leaves processed options in All.
%  * process(Proccess)
%    with Proccess in 
%    * debug     will turn on debugging according to debug/0,1,2 options, see below
%
% When processing debugging options in All, the first matching term of the following is used: 
%   * debug   short for debug(true)
%   
%   * debug(Dbgs)  short for debug(Dbgs,_Prior)
%
%   * debug(Dbgs,Prior)  Prior is the prior status of debug(PredName). For each element 
% of Dbgs call RHS:
%     * true   debug(PredName)
%     * false  nodebug(PredName)
%     * none   true
%     * all    debug(_)
%     * Other  debug(Other)
%
%== 
% ?- assert( demo_defaults(ls(true)) ).
% ?- options_append( demo, ls(false), All ).
% All = [ls(false), ls(true)].
% 
% ?- options_append( demo, debug, All, process(debug) ).
% All = [ls(true)].
% 
% ?- options_append( demo, [debug(true),ls(false)], All, [process(debug),debug(true)] ).
% Turning debugging on for predicate handle: demo
% All = [ls(false), ls(true)].
% 
% % Note that the debug(options_append) has been removed.
% ?- options_append( demo, debug, All, process(debug) ).
% All = [ls(true)].
%==
% 
% The default OAopts list is [funnel(debug)].
% 
% @author nicos angelopoulos
% @version  0.2 2014/9/20
% @tbd  add option! for making sure that only recognised options (with additional,"silent" defaults) are accepted ??
% @see ~/bin/cline/keep.pl for debug option example
% @tbd allow for strict lists inputs
%
options_append( Pname, Args, Opts ) :-
	options_append( Pname, Args, Opts, [funnel(debug)] ).
options_append( Pname, ArgS, Opts, OAoptS ) :-
	atom_concat( Pname, '_defaults', Dname ),
	en_list( ArgS, Args ),
	options_append_profile_options( Pname, Args, ProfArgs ),
	en_list( OAoptS, OAopts ),
	% options_append_args( OAopts, Args, Arity ),
	% options_def_append( Dname, Pname, Opts, Args, Arity, Defs, All ),
	findall( Xarg, member(extra_arg(Xarg),OAopts), Xargs ),
	append( ProfArgs, Xargs, ExtArgs ),
	options_def_append( Dname, Pname, ProfArgs, ExtArgs, Defs, OptsUnpro ),
	options_append_select_own_debug( OAopts, ProcessOpts, Restore ),
	options_append_process( ProcessOpts, OptsUnpro, Defs, Pname, Opts ),
	options_append_restore_debug_status( Restore ).

% options_append_profile_options( Pname, Args, ProfArgs ),
% If file $HOME/.pl/Pname.pl exists, append its terms to Args.
%
options_append_profile_options( Pname, Args, Semi ) :-
	atomic_list_concat( ['$HOME/.pl/',Pname,'.pl'], Hfile ),
	expand_file_name( Hfile, [File] ),
	exists_file( File ),
	read_file_to_terms( File, Terms, [] ),
	append( Args, Terms, Semi ),
	!.
options_append_profile_options( _Pname, Args, Args ).

/*
options_append_args( Opts, ArgsList, Arity ) :-
	memberchk( args(Args), Opts ),
	!,
	ArgsList = [Args],
	Arity is 2.
options_append_args( _Opts, Args, Arity ) :-
	Args = [],
	Arity is 1.
	*/

options_append_process( [], Opts, _Defs, _Pname, Opts ).
options_append_process( [extra_arg(_)|T], All, Defs, Pname, Opts ) :-
	!,
	options_append_process( T, All, Defs, Pname, Opts ).
options_append_process( [process(Opt)|T], All, Defs, Pname, Opts ) :-
	!,
	select_all( T, process(Opt), _, Rem ), % remove dublicates % should Opt by anonymous here?
	options_append_process_option( Opt, All, Pname, Nxt, _Enh, Opts ),
	options_append_process( Rem, Nxt, Defs, Pname, Opts ).
options_append_process( [funnel(Opt)|T], All, Defs, Pname, Opts ) :-
	!,
	select_all( T, process(Opt), _, Rem ),
	options_append_process_option( Opt, All, Pname, _Nxt, Enh, Rem ),
	options_append_process( Rem, Enh, Defs, Pname, Opts ).
options_append_process( [foreign(Fgn)|T], All, Defs, Pname, Opts ) :-
	!,
	select_all( T, foreign(_), _, Rem ), % remove dublicates 
	exclude( template_in_defaults(Defs), All, Fgn ),
	options_append_process( Rem, All, Defs, Pname, Opts ).
options_append_process( [Opt|_T], _All, _Defs, Pname, _Opts ) :-
	throw( unknown_option_in_options_append(Opt,Pname) ). % fixme

template_in_defaults( Defs, Term ) :-
	term_template( Term, Template ),
	memberchk( Template, Defs ).

options_append_process_option( Opt, All, Pname, Nxt, Enh, Opts ) :- 
	options_append_known_process_option( Opt ),
	!,
	options_append_option_process( Opt, All, Pname, Nxt, Enh, Opts ).
options_append_process_option( Opt, _All, _Pname, _Nxt, _Enh, _Opts ) :- 
	throw( options_append( unknown_process_option(Opt)) ).

% user + program can use debug/0,1,2 the first one 
% is only used, the Dbg terms argument can be a list if 
% more subjects need debugging
% 
% fixme: this doesn't work properly from multi_debugs
%  
options_append_option_process( debug, All, Pname, NxtEnh, Enh, _Opts ) :-
	partition( name_term(debug), All, Dbgs, Nxt ),
	Dbgs = [Dbg|_],
	!,
	( debugging(Pname) -> Status = true; Status = false ),
	Rst = '$restore'(Pname,debug,Status),
	Enh = [Rst|All],
	NxtEnh = [Rst|Nxt],
	options_append_option_process_debug( Dbg, Pname ).
% next clause states that we shouldn't complain if there is no debug/1,2,3
options_append_option_process( debug, All, _Pname, Nxt, Enh, _Opts ) :-
	% fixme: add option_append option for strictness here ?
	Nxt = All,
	Enh = All.
	
options_append_option_process_debug( debug, Pname ) :-
	options_append_option_process_debug_arg( Pname, true ).
options_append_option_process_debug( debug(DbgS), Pname ) :-
	en_list( DbgS, Dbgs ),
	maplist( options_append_option_process_debug_arg(Pname), Dbgs ).
options_append_option_process_debug( debug(DbgS,Prior), Pname ) :-  % fixme, this is now dealt by '$restore' ?
	en_list( DbgS, Dbgs ),
	debugging_status( Pname, Prior ),
	maplist( options_append_option_process_debug_arg(Pname), Dbgs ).

options_append_option_process_debug_arg( _, Var ) :-
	var( Var ),
	!,
	throw( options_append( instantiation_of_debug) ).
/*
options_append_option_process_debug_arg( Pname, _ ) :- !,
	\+ prolog_debug:debug_topic(Pname).
	*/
options_append_option_process_debug_arg( _Pname, none ) :- !.
options_append_option_process_debug_arg( Pname, false ) :- !,
	nodebug( Pname ).
options_append_option_process_debug_arg( Pname, true ) :- !,
	( prolog_debug:debug_topic(Pname) -> true; prolog_debug:debug_topic(Pname) ),
	debug( options_append, 'Turning debugging on for predicate handle: ~w', [Pname] ),
	debug( Pname ).
options_append_option_process_debug_arg( _Pname, all ) :- !,
	debug( options_append, 'Turning indescriminate debugging on', [] ),
	debug( _ ).
options_append_option_process_debug_arg( _, Other ) :- !,
	debug( options_append, 'Turning debugging on for handle: ~w', [Other] ),
	debug( Other ).

options_def_append( Dname, Pname, Args, ExtArgs, Defs, All ) :-
	predicate_name_defined_in( Pname, Mod ),
	debug( options, 'Defined in: ~w', [Mod:Pname] ),
	member( Arity-Gargs, [1-[DefS],2-[ExtArgs,DefS]] ),
	current_predicate( Mod:Dname/Arity ),
	!,
	% append( Left, [DefS], Args ),
	Goal =.. [Dname|Gargs],
	call( Mod:Goal ),
	en_list( DefS, Defs ),
	append( Args, Defs, All ).
options_def_append( Dname, _Pname, Args, Defs, All ) :-
	current_predicate( Dname/Arity ),
	member( Arity-Gargs, [1-[DefS],2-[Args,DefS]] ),
	!,
	% append( Left, [DefS], Args ),
	Goal =.. [Dname|Gargs],
	call( Goal ),
	en_list( DefS, Defs ),
	append( Args, Defs, All ).
options_def_append( Dname, Pname, Opts, [], All ) :-
	Mess = 'Could not locate defaults predicate: ~w, in modules: ~w',
	message_report( Mess, [Dname/'1,2',[user,Pname]], warning ),
	All = Opts.

options_append_select_own_debug( Opts, NoDebug, Restore ) :-
	select_first( Opts, debug(Dbg), NoDebug ),
	memberchk( Dbg, [true,false] ),
	!,
	debugging_status( options_append, Restore ),
	options_append_debug_own( Dbg ).
options_append_select_own_debug( Opts, Opts, true ).

options_append_debug_own( true ) :-
	debug( options_append ).
options_append_debug_own( false ).

options_append_restore_debug_status( true ).
options_append_restore_debug_status( false ) :-
	nodebug( options_append ).

predicate_name_defined_in( Pname, Mod ) :-
	current_module( Mod ),
	current_predicate( Mod:Pname/_).
predicate_name_defined_in( Pname, Mod ) :-
	current_module(In),
	current_predicate(In:Pname/N),
	length(List,N),
	Goal =.. [Pname|List],
	predicate_property(In:Goal,imported_from(Mod)).
