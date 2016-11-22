
:- use_module( library(os_lib) ).		% os_postfix/4.

%% chain_apply( +Pred, +Metrics, +TimesOp, +File, +PsfOpts, ?Out ).
%
% Apply Pred + num_of(Metrics) args to each model in File.
% File should be a chain file storing model facts of the form 
% m(Tms,Model).
% 
% Metrics is the number of outputs to expect from Pred. The corresponding
% number of variables is added before calling Pred at each iteration.
%
% TimesOp defines how to apply the times the model appeared consequtively (Tms) 
% to the output(s)- use =|false|= for no transformation. TimesOp should be the 
% functor name of predicate with arity 3. (First is Tms, second is all Metrics
% (input) and 3rd the corresponding Metrics outputs.
%
% PsfOpts are the options passed to os_postfix/4 to produce Out from File.
% When Out is a variable and PsfOpts == false, then Out is instantiated to 
% the list of the terms produced. When Out is a variable and PsfOpts \== false,
% Out is the 3rd argument of the call =|os_postfix( _, File, Out, PsfOpts ).|=
% In this case PsfOpts should at least contain postfix(Psfx).
%
% TimesOp can be either multi_times, or a user 3-arity predicate.
%
chain_apply( Pred, Metrics, TimesOp, File, Opts, OutTo ) :-
	open( File, read, In ),
	chain_apply_out_open( OutTo, File, Opts, Out ),
	read( In, Term ),
	chain_apply_term_stream( Term, Pred/Metrics, In, TimesOp, Out ),
	chain_apply_out_close( Out ).

chain_apply_term_stream( end_of_file, _Pred, _In, _Top, [] ) :- !.
chain_apply_term_stream( m(Tms,Model), Pred, In, Top, Out ) :-
	!,
	chain_apply_model( Model, Pred, Tms, Top, Out, Otail ),
	read( In, InTerm ),
	chain_apply_term_stream( InTerm, Pred, In, Top, Otail ).
chain_apply_term_stream( Term, Pred, _In, _Top, Out ) :-
	( is_stream(Out) -> close(Out); true ),
	throw( unrecognised_model_term_in_chain_apply_input_stream(Term,Pred) ).
	% fixme

chain_apply_model( Model, Pred/Metrics, Tms, Top, Out, Otail ) :-
	% debug( _, 'Model: ~w', [Model] ),
	length( Args, Metrics ),
	Pred =.. [Pname|Pargs],
	append( Pargs, [Model|Args], Aargs ),
	Goal =.. [Pname|Aargs],
	call( Goal ),
	chain_apply_times_op( Top, Tms, Args, Timed ),
	chain_apply_record_on( Out, Timed, Otail ),
	!. % fixme, we only use this to throw error below,
	   % but inadvertently we cut choise points in this clause
	   % that altough they shouldnt be there, we should know about if there are
chain_apply_model( Model, Pred/Metrics, Tms, Top, _Out, _Otail ) :-
	throw( cannot_apply_to_model(Pred,Metrics,Model,Tms,Top) ).

chain_apply_times_op( Top, Tms, Args, Timed ) :-
	call( Top, Tms, Args, Timed ).

chain_apply_record_on( Out, Timed, Otail ) :-
	is_stream( Out ),
	!,
	portray_clause( Out, Timed ),
	Otail = Out.
chain_apply_record_on( Out, Timed, Otail ) :-
	Out = [Timed|Otail].

chain_apply_out_open( OutTo, _File, Opts, Out ) :-
	var( OutTo ),
	Opts == false,
	!, % Pass OutTo to Out becoming the list of results
	Out = OutTo.
chain_apply_out_open( OutTo, File, Opts, Out ) :-
	os_postfix( _, File, OutTo, Opts ),
	open( OutTo, write, Out ).

chain_apply_out_close( Out ) :-
	is_stream( Out ),
	!,
	close( Out ).
chain_apply_out_close( _Out ).

multi_times( Tms, List, TmsList ) :-
	maplist( *(Tms), List, TmsList ).

'*'(X,Y,Prod) :- Prod is X * Y.
