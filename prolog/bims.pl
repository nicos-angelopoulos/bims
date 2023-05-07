:- module( bims,  [
                         bims/0, bims/1,
                         bims_version/2, bims_citation/2,
                         dlp_load/1, dlp_load/2,
                         dlp_sample/1,dlp_sample/3,
                         dlp_call/1,dlp_call/3,
                         dlp_call_sum/2,
                         dlp_seed/0, dlp_path_prob/2, dlp_path_prob/3
                         ] ).

:- use_module( library(lib) ).
% :- ensure_loaded( '../src/mcmcms' ).
% :- ensure_loaded( '../src/init_lib' ).
:- lib(source(bims), homonyms(true)).

% :- lib(stoics_lib).  % en_list/2, goal_spec/2, portray_clauses/2. 
% the following could be had from pack(stoics_lib)
:- lib(goal_spec/2).
:- lib(portray_clauses/2).

:- lib(mcmcms/12).
:- lib(os_unique_by_date/2).
:- lib(n_digits/3).
:- lib(clean_module/1).
:- lib(report_triggers/1).
:- lib(bims_bb_remove/2).
:- lib(remove_template_duplicates/2).
:- lib(get_date_time/1).
:- lib(ord_only_add_elem/3).  % needed on ad_expand
:- lib(dlp_load/1).
:- lib(dlp_sample/1).
:- lib(dlp_call/1).
:- lib(dlp_path_prob/2).
:- lib(dlp_seed/0).
:- lib(end(bims)).
% :- lib_pack_end( bims ).


/** <module> Bims- Bayesian inference over model structures.

---++ Introduction
Bims (Bayesian inference over model structures) implements MCMC learning
over statistical models defined in the Dlp (Distributional logic programming)
probabilistic language. 

Bims is released under GPL2, or Artistic 2.0

Currently there are 2 model spaces supported: 
    * Carts (Classification & Regression trees), and
    * Bayesian Networks

Additional model spaces can be easily implemented by defining new likelihood plug-ins
and programming appropriate priors.


---+++ Examples provided

---+++ Carts examples

==
?- bims( [] ).
?- bims( [data(carts),models(carts),likelihood(carts)] ).
==

The above are two equivalent ways to run the Carts example provided. 

This runs 3 chains each of length 100 on the default Carts data using the default
likelihood. The default dataset is the breast cancer Winsconsin (BCW) data from 
the machine learning repository. There are 2 categories, 9 variables and 683 data points
in this dataset. You can view the data with
==
?- edit( pack(bims/data/carts) ).
==

The default likelihood is an implementation of the classification likelihood function presented in: 
H Chipman, E George, and R McCulloch. Bayesian CART model search (with
discussion). J. of the American Statistical Association, 93:935–960, 1998.

---+++ Bns examples

==
?- bims( [models(bns)] ).
?- bims( [data(bns),models(bns),likelihood(bns)] ).
==
The above are two equivalent ways to run the Bns example provided. 

This runs 3 chains each of length 100 on the default bns data using default likelihood.
The dataset is a sampled dataset from the ASIA network and it comprises of 8 variables and 
2295 datapoints. You can view the data with
==
?- edit( pack(bims/data/bns) ).
==

The default BN likelihood is an instance of the BDeu metric for scoring BN structures.

W. L. Buntine. Theory refinement of Bayesian networks. In Bruce D’Ambrosio, Philippe
Smets, and Piero Bonissone, editors, Proceedings of the Seventh Annual Conference on
Uncertainty in Artificial Intelligence (UAI–1991), pages 52–60, 1991

David Heckerman, Dan Geiger, and David M. Chickering. Learning Bayesian networks:
The combination of knowledge and statistical data. Machine Learning, 20(3):197–243,
1995.

---+++ Learning models from new datasets

An easy way to run Bims on your data is to create a new directory and within 
that sub-directory data/ copy your data there and pass options data/1 to the basename
of the data file. 

For example, 
==
?- bims( data(mydata) ).
==

---+++ Learning new statistical models.

By defining a new likelihood function and new priors 
the system can be used on new statistical models.

---++ Pack info

@author Nicos Angelopoulos, http://stoics.org.uk/~nicos
@author James Cussens (University of York), http://cs.york.ac.uk/~jc
@version  2.0 2017/02/21, IJAR paper
@version  2.1 2017/03/10, pack lib
@version  2.2 2017/04/18, web-doc; de-git
@version  2.3 2018/12/21, aux/ -> aux_code
@version  2.4,2021/12/29, run on SWI 8.5.4; github core complete
@version  2.5,2022/01/02, src/lib clean-up
@see http://stoics.org.uk/~nicos/sware/bims
@tbd bims_default(-Def).
@tbd test on Windows (and Mac ?)
@license MIT
*/

bims_defaults( ArgsPrv, [    
                chains(3), 
                report([]),
                results_dir(Dir),
                iterations(100),
                tempered([]),
                seeds(1),
                progress_percentage(10),
                progress_stub('.'),
                DefModelT,
                prior(ModelSingular),
                likelihood(Model),
                data(Model),
                backtrack(uc),
                top_goal(ModelSingular),
                debug(true)
        ] ) :-
               en_list( ArgsPrv, Args ),
               ( (memberchk(results_dir(ArgsDir),Args),ground(ArgsDir)) ->
                    make_directory(ArgsDir)
                    ;
                    true
               ),
               DefModel = carts,
               DefModelT = models(DefModel),
               append( Args, [DefModelT], Partial ),
               memberchk( models(Model), Partial ),
               atom_singular( Model, ModelSingular ),
               bims_args_results_dir( Args, Dir ).

/** bims.
    bims( +File ).
    bims( +Opts ).

Run a number of MCMC runs for a single prior defined by a Distributional Logic Program (DLP). 

If the argument (File) corresponds to an existing file, then it is taken to be a settings file.
Each argument should be a fact correspond to a known option.
For example 
==
chains(3).
iterations(100).
seeds([1,2,3]).
==

If the argument (Opts) does not correspond to a file is take to be a list of option terms.

The simplest way to use the software is to make a new directory and run some MCMC chains.
The default call,
==
?- bims().    % equivelant to ?- bims([]).
==
runs a 3 chains (R=3, below) 100 iterations (I=100) MCMC simulation.
The models learnt are classifications trees (carts) based on the default prior
and the data are the BCW dataset.
The above call is equivelant to:
==
?- bims([models(carts)]).
==

To run a toy BN learning example run
==
?- bims( [models(bns)] ).
==

This runs 3 chains on some synthetic data of the 8-nodal Asia BN.

To get familiar on how to run bims on private data, make a new directory, 
create a subdirecory =|data|= and copy file bims(data/asia.pl) to 
data/test_local.pl.
==
?- bims( [data(test_local)] ).
==

Opts 
  * chains(R=3)
    number of chains or runs. Each chain is identified by N in 1...R.
  * iterations(I=100) 
    number of iterations per run. Strictly speaking this is iterations - 1. 
    That is: I is the number of models in each chain produced.
  * models(Models=carts) 
    type of the models in the chain. An alternative type of model type is =|bns|=.
  * debug(Dbg=true)
    If Dbg==true, run debug(bims) to get debuging messages. If Dbg==false, nodebug(bims) is called.
  * seeds(Seeds=1)
    hash seeds for each run (1-1000), if length of Seeds is less than R, additional items added consequtively 
    from last value.  So for instance, seeds(1) when chains(3) is given expands to seeds([1,2,3]).
  * likelihood(Lk=Model)
    likelihood to use, default depends on Model chosen (system provided models, have a nameshake 
    default likelihood, for example _carts_ likelihood is the default likelihood for carts models)
  * data(Data=Model)   
    a term that indicates the data for the runs. The precise way of loading and calls depend on 
    Lk (the likelihood function) via the hook model_data_load/2, and what the prior 
    (see option top_goal(Top)) expects. In general the dependency is with the likelihood, 
    with the prior expected to be compatible with what the likelihood dictates in terms of data.
    In the likelihoods provided, Data is the stem of a filename that is loaded in memory.  
    The file is looked for in Dir/Data[.pl] where Dir is looked for in [./data,bims(Model/data/)].
  * top_goal(Top=Model) 
    the top goal for running the MCMC simulations. Should be the partial call corresponding 
    to a predicate defined in Prior, as completed by adding the model as the last argument.
  * prior(Prior=Model) 
    a file defining the prior DLP. Each model space has a default nameshake prior. 
    The prior file is looked for in _dlps_ and bims(dlps).
  * backtrack(Backtract=uc)
    backtracking strategy (fix me: add details)
  * tempered(Tempered=[])   
    hot chains (fixme: add details) - this is an advanced feature undocumented for now
  * results_dir(Rdir=res-Dstamp)  
    results directory. If absent default is used. If present but a variable the default is 
    used and returned as the instantiation to this variable.
    The directory should not exist prior to the call. 
    The default method uses a time stamp to provide uniqueness. (fixme: add prefix(Pfx) recognition)
  * report(These)
    where These is a listable set of reportable tokens (should match 1st argument of bims:known_reportable_term/2). 
    =[all|_]  or =all= is expanded to reporting all known reportable terms.
  * progress_percentage(Pc=10) 
    the percentage at which to report progress of all runs (>100 or non numbers for no progress reporting)
  * progress_stub(Stub='.')
    the stub marking progress

All file name based options: Lk, Data, Prior or Rdir, are passed through absolute_file_name/2.

The predicate generates one results directory (Rdir) and files recording information about each run (R) are placed in Rdir.

*/
bims :- bims( [] ).

bims( ArgsPrv ) :-
    en_list( ArgsPrv, Args ),
    bims_defaults( Args, Defs ),
    append( Args, Defs, Opts ),
    debug( bims, 'Bims options: ~w', [Opts] ),
    bims_option_debug( Opts, DbgRestore ),
    memberchk( results_dir(ResDPrv), Opts ),
    absolute_file_name( ResDPrv, ResD ),
    debug( bims, 'Results directory: ~w', ResD ),
    memberchk( chains(Runs), Opts ),
    bims_option_seeds( Opts, Seeds ),
    remove_template_duplicates( Opts, UnqOpts ),
    number_codes( Runs, Rcodes ),
    length( Rcodes, RcLen ),
    n_digits( RcLen, 0, Zero ),
    atomic_list_concat( [Zero,'opts.pl'], '-', BoptsB ),
    directory_file_path( ResD, BoptsB, BroF ),
    bims_write_options( BroF, UnqOpts ),
    % fixme: likel
    % likelihood(Lk=Model)
    % model(Mdl), 
    PersOpts = [ models(Model), prior(Dlp), likelihood(Lkl), 
                 data(Data), top_goal(Goal),
               report(These)
               ],
    maplist( list_element(Opts), PersOpts ),
    report_triggers( These ),
    bims_ensure_likelihood_loaded( Model, Lkl, AbsLkl ),
    bims_ensure_data_loaded( Model, Lkl, Data, AbsData ),
    bims_locate_prior_file( Model, Dlp, AbsDlp ),
    bims_write_abs_options( BroF, abs(AbsLkl,AbsData,AbsDlp) ),
    bims_runs( Runs, 1, RcLen, Model, AbsDlp, Seeds, ResD, Goal, Opts ),
    get_date_time( Now ),
    portray_clauses( [finished_at(Now)], [file(BroF),mode(append)] ),
    bims_option_debug_set( DbgRestore ).

bims_runs( 0, _I, _Dgs, _Mdl, _, _Seelds, _ResD, _Goal, _Opts ) :-
    !,
    debug( bims, 'Finished all runs', [] ).
bims_runs( R, I, Dgs, Mdl, Dlp, [Seed|Seeds], ResD, Goal, Opts ) :-
    copy_term( Goal, TopG ),
    % * Out, *Stats, Kernel, ModelType, Prior, PB, Repeats, Data, HotChainsIds, *Seed, SGl
    % Out = chain_output( 01-chain.pl ), Stats= 01_stats.txt
    debug( bims, 'Run: ~d', I ),
    n_digits( Dgs, I, Iat ),
    bims_out_file( ResD, Iat, chain, pl, Chain ),
    bims_out_file( ResD, Iat, stats, pl, Stats ),
    bims_out_file( ResD, Iat, report, pl, Rep ), % fixme: nicer name ?
    MCopts = [ backtrack(BckT), iterations(Its), tempered(Hot), 
               progress_percentage(Pc), progress_stub(Stub) 
            ],
    maplist( list_element(Opts), MCopts ), %fixme: allow lists
    bims_progress_reporter( Its, Pc, Stub ),
    bims_backtrack_term( BckT, Bck, P ),
    mcmcms( Chain, Stats, Rep, Bck, Mdl, Dlp, P, Its, true, Hot, Seed, TopG ),
    !, % fixme: do some cleaning
    J is I + 1,
    Q is R - 1,
    bims_runs( Q, J, Dgs, Mdl, Dlp, Seeds, ResD, Goal, Opts ).

/** bims_version( -Vers, -Date ).

Version Mj:Mn:Fx, and release date date(Y,M,D).

==
?- bims_version(Vers, Date).
Vers = 2:5:0,
Date = date(2022, 1, 2).
==

@see doc/Releases.txt for more detail on change log
@version 2:5:0

*/
bims_version( 2:5:0, date(2022,1,2) ).

/** bims_citation( -Atom, -Bibterm ).

Succeeds once for each publication related to this library.
Atom is the atom representation suitable for printing while Bibterm 
is a bibtex(Type,Key,Pairs) term of the same publication. 
Produces all related publications on backtracking.

?- bims_citation( A, G ), write( A ) nl.

Distributional Logic Programming for Bayesian Knowledge Representation. 

Nicos Angelopoulos and James Cussens. 

International Journal of Approximate Reasoning (IJAR).

Volume 80, January 2017, pages 52-66.

*/

bims_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom = 'Distributional Logic Programming for Bayesian Knowledge Representation. 
Nicos Angelopoulos and James Cussens. 
International Journal of Approximate Reasoning (IJAR).
Volume 80, January 2017, pages 52-66.',
    Type = article,
    Key  = 'AngelopoulosN_CussensJ_2017',
    Pairs = [
               author = 'Nicos Angelopoulos and James Cussens',
               title  = 'Distributional Logic Programming for Bayesian Knowledge',
               journal= 'International Journal of Approximate Reasoning',
               year   = 2017,
               month  = 'January',
               volume = 80,
               pages  = '52-66',
               url    = 'http://www.sciencedirect.com/science/article/pii/S0888613X16301232'
     ].
bims_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom  = 'Bayesian learning of Bayesian Networks with informative priors.
Nicos Angelopoulos and James Cussens (2008).
Special issue on BN learning. Annals of Mathematics and Artificial Intelligence (AMAI) 54(1-3), 53-98.',
    Type  = article,
    Key   = 'AngelopoulosN_Cussens_2008',
    Pairs = [
               author = 'Nicos Angelopoulos and James Cussens',
               title  = 'Bayesian learning of Bayesian Networks with informative priors.',
               year   = '2008',
               journal= 'Special issue on BN learning. Annals of Mathematics and Artificial Intelligence (AMAI)',
               volume = '54',
               issue  = '1-3',
               pages  = '53-98',
               url    = 'http://dx.doi.org/10.1007/s10472-009-9133-x'
           ].
bims_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom    = 'Exploiting Informative Priors for Bayesian Classification and Regression Trees.
Nicos Angelopoulos and James Cussens (2005)
In 19th International Joint Conference on Artificial Intelligence (IJCAI-05), 641-646, Edinburgh, UK, August 2005.',
    Type    = inproceedings,
    Key     = 'AngelopoulosN_Cussens_2005',
    Pairs   = [
                title   = 'Exploiting Informative Priors for Bayesian Classification and Regression Trees',
                author  = 'Nicos Angelopoulos and James Cussens',
                year    = '2005',
                inproceedings = 'In 19th International Joint Conference on Artificial Intelligence (IJCAI-05)',
                pages   = '641-646',
                address = 'Edinburgh, UK',
                month   = 'August',
                year    = 2005
            ].
bims_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom    = 'Markov chain Monte Carlo using tree-based priors on model structure.
Nicos Angelopoulos and James Cussens (2001).
In 17th Conference on Uncertainty in Artificial Intelligence (UAI-2001), 16-23, Seattle, USA.',
    Type    = inproceedings,
    Key     = 'AngelopoulosN_Cussens_2001',
    Pairs   = [
                title   = 'Markov chain Monte Carlo using tree-based priors on model structure.',
                author  = 'Nicos Angelopoulos and James Cussens',
                year    = 2001,
                proceedings = 'In 17th Conference on Uncertainty in Artificial Intelligence (UAI-2001)',
                pages   = '16-23',
                address = 'Seattle, USA'
            ].

bims_lib( Spec ) :-
    ( Spec = Fname/_Arity -> true; Spec = Fname ),
    ensure_loaded( bims(src/lib/Fname) ).

%% bims_option_seeds( +Opts, -Seeds ).
% 
%  Get the seed ids (Sids) to be used with runs from options, Opts.
%  Ensures Sids is at least as long as runs in Opts.
%
bims_option_seeds( Opts, Seeds ) :-
    memberchk( chains(Runs), Opts ),
    memberchk( seeds(PrvSeedS), Opts ),
    en_list( PrvSeedS, PrvSeeds ),
    length( PrvSeeds, PrvLen ),
    Diff is max( Runs - PrvLen, 0 ),
    last( PrvSeeds, Last ),
    findall( A, (  between(1,Diff,I),
                 A is Last + I
                ), As ),
    append( PrvSeeds, As, Seeds ).

%% bims_option_debug( Opts, -Bef ).
%
%  Start debugging messages if debug(true) is in Opts. 
%  Before indicates the state of bims debugging prior to the call.
%  The values for Before are: false or true.
%
bims_option_debug( Opts, Bef ) :-
    memberchk( debug(Dbg), Opts ),
    debugging( bims, Bef ),
    !,
    bims_option_debug_set( Dbg ).

bims_option_debug_set( true ) :-
    debug( bims ).
bims_option_debug_set( false) :-
    nodebug( bims ).

%% bims_out_file( +ResD, +Iat, +Tkn, +Ext, +File ).
% 
% Construct a bims output File from a results directory, an iteration atom (Iat) a token (Tkn) and an extension (Ext).
%==
% ?- bims:bims_out_file( dir, '01', chain, pl, File ).
% File = 'dir/01-chain.pl'.
%==
%
bims_out_file( Dir, Iat, Tkn, Ext, File ) :-
    atomic_list_concat( [Iat,Tkn], '-', Bstem ),
    file_name_extension( Bstem, Ext, Bname ),
    directory_file_path( Dir, Bname, File ).

bims_ensure_data_loaded( Model, Lkl, DataT, DataF ) :-
    DataT =.. [Data|Args],
    % generic section
    clean_module( data ),
    AbsOpts = [file_type(directory),solutions(all)],
    findall( BimsDD, absolute_file_name(bims(data),BimsDD,AbsOpts), BimsDDs ),
    debug( bims, 'Bims data dirs: ~w', [BimsDDs] ),
    member( Dir, [data|BimsDDs] ),
    directory_file_path( Dir, Data, Stem ),
    file_name_extension( Stem, pl, DataF ),
    debug( bims, 'Testing existance of data file: ~w', DataF ),
    exists_file( DataF ),
    debug( bims, 'Loading data file: ~w', DataF ),
    data:load_files( DataF, [silent(true)] ),
    % data:ensure_loaded( DataF ),
    bims_data_call( Model, Lkl, Args, PrepF, PrepG ),
    assert( data:data_file(DataF) ),
    assert( data:data_models(Model) ),
    assert( data:data_prep_file(PrepF) ),
    assert( data:data_prep_goal(PrepG) ),
    !.
bims_ensure_data_loaded( Model, Lkl, Data, _DataF ) :-
    throw( fixme(cannot_load_data_for(Model,Lkl,Data)) ).

bims_ensure_likelihood_loaded( Model, Lkl, PlLkl ) :-
    Sub = models/Model/lklhoods/Lkl/Lkl,
    clean_module( bims_lkl ),
    absolute_file_name( bims(Sub), LklStem, [solutions(all)] ),
    file_name_extension( LklStem, pl, PlLkl ),
    debug( bims, 'Looking for likelihood in file: ~w', PlLkl ),
    exists_file( PlLkl ),
    debug( bims, 'Loading likelihood from file: ~w', PlLkl ),
    % ensure_loaded( bims:PlLkl ),
    bims_lkl:load_files( PlLkl, [silent(true),if(true)] ),
    !.
bims_ensure_likelihood_loaded( Model, Lkl, _ ) :-
    throw( fixme(cannot_find_likelihood_for(Lkl,Model)) ).

bims_data_call( Model, Lkl, Args, DataMan, GoalCopy ) :-
    atom_concat( Lkl, '_data', BaseStem ),
    file_name_extension( BaseStem, pl, BaseName ),
    Sub = models/Model/lklhoods/Lkl,
    AbsOpts = [file_type(directory),solutions(all)],
    absolute_file_name( bims(Sub), LklD, AbsOpts ),
    directory_file_path( LklD, BaseName, DataMan ),
    debug( bims, 'Looking data preparation file in: ~w', DataMan ),
    exists_file( DataMan ),
    debug( bims, 'Loading data preparation file: ~w', DataMan ),
    bims_data:load_files( DataMan, [silent(true),if(true)] ),
    Goal =.. [BaseStem,Args],
    duplicate_term( Goal, GoalCopy ),
    call( bims_data:Goal ).

list_element( List, Element ) :-
    memberchk( Element, List ).

bims_backtrack_term( Term, Bck, P ) :-
    functor( Term, Bck, Arity ),
    bims_backtrack_arity_term( Arity, Term, P ).
    % fixme: add Arity > 1 error

bims_backtrack_arity_term( 0, Atom, Atom ).
bims_backtrack_arity_term( 1, Term, Arg ) :-
    arg( 1, Term, Arg ).

bims_locate_prior_file( Model, DlpStem, AbsDlp ) :-
    AbsOpts = [file_type(directory),solutions(all)],
    absolute_file_name( bims(models/Model/dlps), Bims, AbsOpts ),
    member( Dir, ['dlps',Bims] ),
    directory_file_path( Dir, DlpStem, DlpPathStem ),
    file_name_extension( DlpPathStem, dlp, AbsDlp ),
    debug( bims, 'Looking for prior in file: ~w', AbsDlp ),
    exists_file( AbsDlp ),
    debug( bims, 'Will be using prior in file: ~w', AbsDlp ),
    !.
bims_locate_prior_file( Model, DlpStem, _AbsDlp ) :-
    throw( fixme(cannot_locate_prior(Model,DlpStem)) ).

atom_singular( Atom, Singular ) :-
    atom_concat( Singular, s, Atom ),
    !.
atom_singular( Atom, Atom ).

bims_args_results_dir( Args, Dir ) :-
    memberchk( results_dir(ResDir), Args ),
    !,
    Dir = ResDir,
    bims_args_results_dir_given( ResDir, Args ).
bims_args_results_dir( Args, Dir ) :-
    bims_args_results_dir_constructed( Args, Dir ).

bims_args_results_dir_given( ResDir, _Args ) :-
    ground( ResDir ),
    !.
bims_args_results_dir_given( ResDir, Args ) :-
    var( ResDir ),
    !,
    bims_args_results_dir_constructed( Args, ResDir ).
bims_args_results_dir_given( ResDir, _Args ) :-
    throw( partially_instantiated_results_dir(ResDir) ).

bims_args_results_dir_constructed( Args, ResDir ) :-
    ( memberchk(dir_prefix(Pfx),Args) -> true; Pfx = res ),
    !,
    os_unique_by_date( Pfx, ResDir ).

bims_write_abs_options( OptsFile, abs(Lkl,Data,Dlp) ) :-
    open( OptsFile, append, Out ),
    nl( Out ),
    write( Out, '% derived' ), nl( Out ),
    Terms = [ likelihood_file(Lkl),
              data_file(Data),
            prior_file(Dlp)
            ],
    maplist( mcmcms_write_fact(Out), Terms ),
    nl( Out ),
    write( Out, '% date/time stamps' ), nl( Out ),
    get_date_time( Now ),
    mcmcms_write_fact( Out, started_at(Now) ),
    close( Out ).

bims_progress_reporter( Its, Pc, Stub ) :-
    bims_bb_remove( progress, _ ),
    number( Pc ), 
    0 < Pc, Pc < 100, 
    !,
    calc_percentiles( Pc, Pc, Its, [HPtl|TPtiles] ),
    bims_bb_put( progress, pts(Stub,HPtl,TPtiles) ).
bims_progress_reporter( _Its, _Pc, _Stub ).

bims_write_options( BroF, UnqOpts ) :-
    open( BroF, write, Out ), 
    write( Out, '% options' ), nl( Out ),
    maplist( mcmcms_write_fact(Out), UnqOpts ),
    close( Out ).

calc_percentiles( Perc, _Step, _Repeats, Ptiles ) :-
    Perc > 100, 
    !,
    Ptiles = [].
calc_percentiles( Perc, Step, Repeats, [F|M] ) :-
    F is integer( Perc * Repeats / 100 ),
    NxP is Perc + Step,
    calc_percentiles( NxP, Step, Repeats, M ).
