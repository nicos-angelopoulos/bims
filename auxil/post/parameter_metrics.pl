
% :- requires( options_append/3 ).
% :- requires( options/2 ).
:- lib(stoics_lib:en_list/2).
:- lib(atom_sub/2).

parameter_metrics_defaults( Args, Defs ) :-
	Defs = [ 
	         metrics(1),
		    numeric(true),
		    positions(Poss),
		    separator('_')
	],
	% (memberchk(metrics(M),Args) -> true; M is 1),
	( memberchk(parameter_names(PnmS),Args) ->
		en_list(PnmS,Pnms), length(Pnms,P)
		;
		P is 1
	),
	numlist( 1, P, Poss ).

/** parameter_metrics( +ChPred, +RedPred, -PMprs, +Opts ).

Collect metrics from chain files corresponding to runs of bims with 
different parameter settings. First ChPred is applied to all models in 
chain files identified and then RedPred is applied to that output to 
get the overall metric for the 

Chain files are looked for in sub-directories of the current or prefix directory.

Directory names should reflect the parameters used in the bims runs.

Output is in the form of Prms-DirMetricsList pairs.

Opts
	* metrics(M=1)           how many metrics are we collecting
	* numeric(N=true)        should parameters mapped to numbers
	* parameter_names(Pnms)  parameter names
	* positions(Poss=1..N)   positions of parameter in dirname parts (after Pfx)
	* prefix(Pfx)            prefix part(s) of directory names
	* separator(Sep='_')     splits dirname to parts

*/
parameter_metrics( Pname, Rname, PMprs, Args ) :-
	Self = parameter_metrics,
    en_list( ArgS, Args ),
    parameter_metrics_defaults( Args, Defs ),
    append( Args, Defs, Opts ),
	% options_append( Self, Args, Opts ),
	debug( parameter_metrics, 'Options: ~w', [Opts] ),
	parameter_metrics_sub_directory( Opts, Pfx, Old ),
	os_dirs( Dirs ),
	include( prefix_atom(Pfx), Dirs, Prefixed ),
	memberchk( separator(Sep), Opts ), 
    memberchk( numeric(Nmc), Opts ),
    memberchk( metrics(NofMs), Opts ),
	memberchk( positions(PosS), Opts ),
	en_list( PosS, Poss ),
	maplist( parameter_metrics_dir(Pfx,Poss,Sep,Nmc,NofMs,Pname,Rname), Prefixed, PMprs ),
	maplist( writeln, PMprs ),
	working_directory( _, Old ).

parameter_metrics_dir( Pfx, Poss, Sep, Nmc, NofMs, Pname, Rname, Dir, Prms-Mtrcs) :-
	atom_concat( Pfx, SepRest, Dir ),
	atom_concat( Sep, Rest, SepRest ),
	at_con( Parts, Sep, Rest ),
	findall( Prm, ( member(Pos,Poss),nth1(Pos,Parts,PrmAtm),
	                numeric_parameter(Nmc,PrmAtm,Prm)
				), Prms ),
	directory_chains_metrics( Dir, NofMs, Pname, Rname, Mtrcs ).

directory_chains_metrics( Dir, NofMs, Pname, Rname, Mtrcs ) :-
	os_dir_files( Dir, Files ),
	include( atom_sub(chain), Files, ChainFiles ),
	maplist( parameter_metrics_file(Dir,Pname,NofMs,Rname), ChainFiles, Mtrcs ).

numeric_parameter( true, Atom, Numb ) :-
	( catch(atom_number(Atom,Numb),_,fail) -> true; Numb = Atom ).
numeric_parameter( false, Atom, Atom).

parameter_metrics_file( Dir, Pname, NofMs, Rname, File, Mtrcs ) :-
	directory_file_path( Dir, File, Path ),
	chain_apply( Pname, NofMs, multi_times, Path, false, ChainMtrcs ),
	call( Rname, ChainMtrcs, Mtrcs ).

parameter_metrics_sub_directory( Opts, Dir, Old ) :-
	options( prefix(Pfx), Opts ),
	options( separator(Sep), Opts ),
	en_list( Pfx, Pfxs ),
	at_con( Pfxs, Sep, Dir ),
	parameter_metrics_existing_directory( Dir, Old ).

parameter_metrics_existing_directory( Dir, Old ) :-
	exists_directory( Dir ),
	!,
	debug( parameter_metrics, 'Descending to: ~p', Dir ),
	working_directory( Old, Dir ).
parameter_metrics_existing_directory( Dir, Old ) :-
	debug( parameter_metrics, 'Remaining in root, as sub-dir missing: ~w', Dir ),
	Old = '.'.
