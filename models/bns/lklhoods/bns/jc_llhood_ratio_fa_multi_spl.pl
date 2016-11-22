% 2005/06/21.
% copied from jc_llhood_ratio_fa_multi, this version
% gives the loglikelihood of a single BN.
% _multi_ because it
% looks for the frequency of data at the last argument of data/n.

% :- requires( bims:kv_compose/3 ).
% :- requires( bims:kvs_insert/4 ).
:- ensure_loaded( 'src/kv_compose' ).
:- ensure_loaded( 'src/kvs_insert' ).
:- ensure_loaded( 'src/kvs_replace_v_or_insert' ).

% note currently the code assumes nodes are represented by 1...n, natural numbers.

% Nicos, 2005/05/21, could we store OldContrib ?
% Nicos, 2004/10/18.
scm_rel_likelihood( NewFamily, OldFamily, RelL ) :-
	llhood_family_contrib( NewFamily, NewContrib ),
	llhood_family_contrib( OldFamily, OldContrib ),
	RelL is exp(NewContrib - OldContrib).

% end of Nicos' code and comments.

/* 
>From jc@cs.york.ac.uk Tue Mar  6 15:53:27 2001
Date: Tue, 6 Mar 2001 11:31:49 GMT
From: jc@cs.york.ac.uk
To: nicos@cs.york.ac.uk

*/

/***********************************************************************

Written Tue Mar  6 11:19:36 GMT 2001

Only llhood_ratio/3 is called from outside
This computes the log of the likelihood ratio of 2 BNs

BNs look like this:

bn(1,[1-[5],2-[],3-[1,5],4-[2],5-[4]]).
bn(2,[1-[5],2-[1,5],3-[4],4-[1],5-[]]).
bn(3,[1-[2,5],2-[5],3-[1,4,5],4-[1,5],5-[]]).

Data (not in this file) looks like this:
data(0,0,1,1,0,1,1,0).

Only works for data in data/8 format

***********************************************************************/

:- use_module(library(ugraphs)).
:- use_module(library(lists)).
:- bims:pl( swi(_), true, use_module(library(terms)) ).
:- use_module(library(ordsets)).

% :- dynamic cached_family_contrib/2.
% :- dynamic cached_family_contrib/3.


% :- 	user:datafile( Data ),
	% user:write( using_data(Data) ), nl,
	% compile( Data ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%START COMPUTATION OF LOG OF LIKELIHOOD RATIO%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------------
  Predicate: llhood_ratio(+BN1,+BN2,-LogofRatio)
       Args: BN1 = Bayesian net as a list of familiies
             BN2 = Bayesian net as a list of familiies
             LogofRatio = log(P(data|BN1)/P(data|BN2))
   Succeeds: 1
   Comments: This is a function
---------------------------------------------------------------------*/

llhood_ratio(BN1,BN2,LogofRatio) :-
	llhood_ratio(BN1,BN2,0,LogofRatio).

/*---------------------------------------------------------------------
  Predicate: llhood_ratio(+BN1,+BN2,+Acc,-LogofRatio)
       Args: BN1 = Bayesian net as a list of familiies
             BN2 = Bayesian net as a list of familiies
	     Acc = accumulator
             LogofRatio = Acc + log(P(data|BN1)/P(data|BN2))
   Succeeds: 1
   Comments: This is a function
             if families are identical then both cancel out, and can be ignored
---------------------------------------------------------------------
Nicos: 2005/06/21, added
*/

llhood_ratio([],[],L,[],L).                 
llhood_ratio([Child-Parents|Rest1],[Child-Parents+Family_Contrib|Rest2],In,StLStr,Out) :- %identical families
	!,
	StLStr = [Child-Parents+Family_Contrib|RestLStr],
	llhood_ratio(Rest1,Rest2,In,RestLStr,Out).
llhood_ratio([Family1|Rest1],[_Family2+Family_Contrib2|Rest2],In,StLStr,Out) :-
	llhood_family_contrib(Family1,Family_Contrib1),
	% llhood_family_contrib(Family2,Family_Contrib2),
	Mid is In + Family_Contrib1 - Family_Contrib2,
	StLStr = [Family1+Family_Contrib1|RestLStr],
	llhood_ratio(Rest1,Rest2,Mid,RestLStr,Out).

/*---------------------------------------------------------------------
  Predicate: llhood_family_contrib(+Family,-Contrib)
       Args: Family = Child-[Parent1,Parent2,...] 
             Contrib = Family's contribution to the log-likelihood
   Succeeds: 1
   Comments: Computes the 'score' for each family
             ie its contribution to the log-likelihood
             Either grab cached value or compute it
             This is a function
---------------------------------------------------------------------*/

% :- ensure_loaded( llhood_family_contrib_direct_enc ).

llhood_family_contrib(Family,Contrib) :-
	% term_hash(Family,HashValue),                
	% (
	    % cached_family_contrib(HashValue,Family,Contrib) ->
	    
	    % true ;
	    
	counts_for_family(Family,ChildCountsList),
	compute_llhood_family_contrib(ChildCountsList,Family,0,Contrib).
	    % assert(cached_family_contrib(HashValue,Family,Contrib))
	% ).
	%Fri Sep 24 10:29:18 BST 2004 debugging jc
	% write(counts_for_family(Family,ChildCountsList)),nl,
	% write(compute_llhood_family_contrib(ChildCountsList,Family,0,Contrib)),nl,nl.

/*---------------------------------------------------------------------
  Predicate: cached_family_contrib(+Family,-Contrib)
       Args: Family = Child-[Parent1,Parent2,...] 
             Contrib = Family's contribution to the log-likelihood
   Succeeds: 1
   Comments: Caches scores for future retreival
             No definition, it gets asserted!
---------------------------------------------------------------------*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%END COMPUTATION OF LOG OF LIKELIHOOD RATIO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%START PREDICATES FOR GETTING COUNTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------------
  Predicate: counts_for_family(+Family,-ChildCountsList)
       Args: Family = Child-[Parent1,Parent2,...] 
             ChildCountsList = for each configuration of the parents
	     a list of labelled counts for child 
   Succeeds: 1
   Comments: This is a function
             Each element of ChildCountsList is something like [1-20,2-30,3-50]-[1,3]
	     Child counts are for Parent1=1, Parent2=3
             [1-20,2-30,3-50] means that the child took value=1 20 times, etc
---------------------------------------------------------------------*/

% Change 3.
counts_for_family(Child-Parents,ChildCountsList) :-
	% Goal = data(_,_,_,_,_,_,_,_),
	%%% data_format( _TheGoal, _Last ),
	% no arg/3 version.
	%% kv_compose( [Child,Last|Parents], [ChildVar,Tms|PVars], Comp ),
	kv_compose( Parents, PaVars, PKVs ),
	%%% Nicos 2005/06/22, keysort is wasteful if Parents are sorted. 
	% keysort( Comp, SortOfInterest ),
	kvs_insert( PKVs, Child, ChVar, SortOfInterest ),
	pairs_with_var_list( SortOfInterest, 1, ArgsList ),
	%%% bb_get( multi_data_format, Goal ),
	%% Goal =.. [data,ArgsList,Tms],
     Goal = data(ArgsList,Tms),
	%% bind([TmsPos,Child|Parents],Goal,[Tms,ChildVar|PVars]),
	% arg( 1, Goal, VarsArg ),
	% arg( 2, Goal, Tms ),
	% ord_add_element( Parents, Child, SortedFamily ),
	% bind_list( SortedFamily, VarsArg, 1, [ChVar|PaVars] ),
	% arg( Last, Goal, Tms ),
	findall(PaVars-(ChVar,Tms),data:Goal,Instances),      % constructs massive list
	child_counts_split( Instances, ChildCountsList, [] ).
	%%% keysort(Instances,SortedInstances),          % order by values of parents
	%%% child_counts_list(SortedInstances,[],ChildCountsList).

child_counts_split( [], Cont, Cont ).
child_counts_split( [PaVals-(ChVal,Tms)|T], [ChCounts-PaVals|TCnts], Cont ) :-
	count_pvals_and_split( T, PaVals, [ChVal-Tms], ChCounts, L, R ),
	child_counts_split( L, TCnts, LCont ), 
	child_counts_split( R, LCont, Cont  ).

count_pvals_and_split( [], _SeenVals, ChCounts, ChCounts, [], [] ).
count_pvals_and_split( [TheseVals-(ChVal,Tms)|T], SeenVals, SoFar, ChCounts, L, R ) :-
	compare( Op, TheseVals, SeenVals ),
	( Op == (=) ->
		kvs_replace_v_or_insert( SoFar, ChVal, Old, New, ThusFar ),
		% ( Old == null__ -> )
		( Old == [] -> 
			New is Tms
			;
			New is Old + Tms
		), 
		TailL = L, TailR = R
		;
		ThusFar = SoFar,
		( Op == (<) -> 
			L = [TheseVals-(ChVal,Tms)|TailL],
			TailR = R
			;
			TailL = L,
			R = [TheseVals-(ChVal,Tms)|TailR]
		)
	),
	count_pvals_and_split( T, SeenVals, ThusFar, ChCounts, TailL, TailR ).

pairs_with_var_list( [], _, _ ).
% pairs_with_var_list( [], _, [] ). % this is only valid if last arg of data
% is known to be in first argument list.
pairs_with_var_list( [K-V|T], N, AList ) :-
	( K =:= N ->
		AList = [V|TAList], R = T
		;
		AList = [_|TAList], R = [K-V|T]
	),
	NxN is N + 1,
	pairs_with_var_list( R, NxN, TAList ).

bind_list( [], _List, _CurrPos, []  ).
bind_list( [Hpos|Tpos], [H|T], CurrPos, [Hplace|Tplaces]  ) :-
	( Hpos =:= CurrPos ->
		Hplace = H, 
		Rpos = Tpos,
		Rplaces = Tplaces
		;
		Rpos = [Hpos|Tpos],
		Rplaces = [Hplace|Tplaces]

	),
	NxPos is CurrPos + 1,
	bind_list( Rpos, T, NxPos, Rplaces ).

/*---------------------------------------------------------------------
  Predicate: bind(+ArgPositions,?Term,+TermList)
       Args: ArgPositions = list of argument positions
             Term = any term
	     TermList = list of subterms of Term
   Succeeds: 1 (unless ArgPositions has an arg outside arity of Term, then fail)
   Comments: This is a function
             Yanks out relevant arguments from Goal and puts them in TermList
---------------------------------------------------------------------*/

bind([],_Goal,[]).
bind([H|T],Goal,[H1|T1]) :-
	arg(H,Goal,H1),
	bind(T,Goal,T1).

/*---------------------------------------------------------------------
  Predicate: child_counts_list(+SortedInstances,+SoFar,-ChildCountsList)
       Args: SortedInstances = sorted instances of a family
             SoFar = accumulator for counts
	     ChildCountsList = list of counts for child, one per parent configuration
   Succeeds: 1
   Comments: This is a function
             Each element of ChildCountsList is something like [1-20,2-30,3-50]-[1,3]
	     Child counts are for Parent1=1, Parent2=3
             [1-20,2-30,3-50] means that the child took value=1 20 times, etc
---------------------------------------------------------------------*/

child_counts_list([],ChildCountsList,ChildCountsList).
child_counts_list([PVals-ChildVal|T],SoFar,ChildCountsList) :-
	child_counts([PVals-ChildVal|T],PVals,[],ChildCounts,Rest),
	child_counts_list(Rest,[ChildCounts-PVals|SoFar],ChildCountsList).


/*---------------------------------------------------------------------
  Predicate: child_counts(+SortedInstances,+PVals,+SoFar,-ChildCounts,-Rest)
       Args: SortedInstances = sorted instances of a family
             PVals = parent configuration
             SoFar = accumulator for counts
	     ChildCounts = list of counts for child, for the PVals parent configuration
	     Rest = Counts for other parent configurations
   Succeeds: 1
   Comments: This is a function
---------------------------------------------------------------------*/

child_counts([PVals-(ChildVal,Tms)|T],PVals,Sofar,Out,Rest) :-        %got a PVals match
% child_counts([PVals-ChildVal|T],PVals,Sofar,Out,Rest) :-        %got a PVals match
	!,
	(
	    select(ChildVal-Count,Sofar,Residue) ->             %if seen this ChildVal before
	    %n C is Count + 1,                                     %increment count
	    C is Count + Tms,                                     %increment count
	    NewSofar = [ChildVal-C|Residue] ;                   %put back
	    NewSofar = [ChildVal-Tms|Sofar]                       %else start the count for this ChildVal
	    % NewSofar = [ChildVal-1|Sofar]                       %else start the count for this ChildVal
	),
	child_counts(T,PVals,NewSofar,Out,Rest).
child_counts(L,_PVals,Out,Out,L).                               %match failed, we're done with this PVals

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END PREDICATES FOR GETTING COUNTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%START COMPUTATION OF FAMILY LOG-LIKELIHOOD%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*---------------------------------------------------------------------
  Predicate: compute_llhood_family_contrib(+ChildCountsList,+Family,+In,-Out)
       Args: ChildCountsList = list of counts for child, one per parent configuration
             Family = ChildName-List of parent names
             In = accumulator input, initially 0
             Out = 'score' for the family
   Succeeds: 1
   Comments: This is a function
             Each element of ChildCountsList is something like [1-20,2-30,3-50]-[1,3]
	     Child counts are for Parent1=1, Parent2=3
             [1-20,2-30,3-50] means that the child took value=1 20 times, etc
             middle loop of Heckerman's (32)
	     Altered: Fri Sep 24 11:43:33 BST 2004
	     to add calls to possible_values/2 and fix_counts/3
	     possible_values/2 is assumed to defined elsewhere.
---------------------------------------------------------------------*/

compute_llhood_family_contrib([],_Family,Out,Out).
compute_llhood_family_contrib([ChildVals-ParentVals|T],Family,In,Out) :-
	Family = Child-_Parents,
	data:possible_values(Child,PossibleValues),                % defined elsewhere
	fix_counts(PossibleValues,ChildVals,FixedChildVals),
	config_contrib(FixedChildVals,Family,ParentVals,In,0,0,Mid),
	compute_llhood_family_contrib(T,Family,Mid,Out).

/*---------------------------------------------------------------------
  Predicate: fix_counts(+PossibleValues,+ChildVals,-FixedChildVals)
       Args: PossibleValues = labelled counts for the child
             ChildVals = labelled counts for the child
	     FixedChildVals = labelled counts for every possible value of the child
   Succeeds: 1
   Comments: Adds explicit zero count if missing. A hack for quick debugging.
             Added by JC Fri Sep 24 11:43:05 BST 2004
---------------------------------------------------------------------*/
/* Nicos 20051124, a more steadfast version. We make sure all child vals
   are in PossValue.
	*/
fix_counts([],ChildVals,[]) :-
	( ChildVals == [] -> true ; 
		throw( impossible_child_values(ChildVals) ) ).
fix_counts([PossValue|T],ChildVals,[PossValue-FixedCount|Rest]) :-
	( bims:nth(_,ChildVals,PossValue-Count,RemChildVals) ->
		FixedCount=Count
		;
		RemChildVals = ChildVals,
		FixedCount is 0
	),
	fix_counts(T,RemChildVals,Rest).

/* James' version, pre 20051124:

fix_counts([],_ChildVals,[]).
fix_counts([PossValue|T],ChildVals,[PossValue-FixedCount|Rest]) :-
	(
	  member(PossValue-Count,ChildVals)
	->
	  FixedCount=Count
	;
	  FixedCount=0
	),
	fix_counts(T,ChildVals,Rest).
	*/

/*---------------------------------------------------------------------
  Predicate: config_contrib(+ChildVals,+Family,+ParentVals,+In,+AIn,+ANIn,-Out)
       Args: ChildVals = labelled counts for the child
             Family = names of child and parents
	     ParentVals = the values of the parents for this configuration
             In = main accumulator input
             AIn = accumulator for Alpha_ij
             ANIn = accumulator for Alpha_ijplusN_ij
             Out = the result!
   Succeeds: 1
   Comments: This is a function
             inner loop of Heckerman's (32)
            Suppose variable x has parents y and z and when y=1,z=3 the counts for x are
            x=1 20 times, x=2 30 times and x=3 50 times, then goal would be:
            config_contrib([1-20,2-30,3-50],x-[y,z],[1,3]In,0,0,Out)
            we just add two accumulators for the sum of alphas and sum of alphas+counts
---------------------------------------------------------------------*/

config_contrib([],_Family,_ParentVals,In,Alpha_ij,Alpha_ijplusN_ij,Out) :-
	/* 	logamma(Alpha_ij,Tmp),
		logamma(Alpha_ijplusN_ij,Tmp2),
		Out is Tmp - Tmp2 + In. */
	Out is lgamma(Alpha_ij) - lgamma(Alpha_ijplusN_ij) + In.
config_contrib([ChildVal-N_ijk|T],Family,ParentVals,In,AIn,ANIn,Out) :-
	alpha(Family,ChildVal,ParentVals,Alpha_ijk),
	Tmp is N_ijk + Alpha_ijk,
	/*	logamma(Tmp,Tmp2),
		logamma(Alpha_ijk,Tmp3),
		Mid is In + Tmp2 - Tmp3,
		*/
	Mid is In + lgamma(Tmp) + lgamma(Alpha_ijk),
	AMid is AIn + Alpha_ijk,
	ANMid is ANIn + Tmp,
	config_contrib(T,Family,ParentVals,Mid,AMid,ANMid,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END COMPUTATION OF FAMILY LOG-LIKELIHOOD


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%START DEFINITION OF PARAMETER PRIORS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Now in alpha_xxx.pl files

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END DEFINITION OF PARAMETER PRIORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





































/**********************************************************************
NOT USED
NOT USED
NOT USED

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% START COMPUTATION OF LIKELIHOOD
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

llhood(BN,L) :-
	llhood(BN,0,L).

% for each family
% does outermost loop of Heckerman's (32)

llhood([],L,L).
llhood([Family|Families],In,Out) :-
	llhood_family_contrib(Family,Family_Contrib),   %this is the 'score' for this family
	Mid is In + Family_Contrib,
	llhood(Families,Mid,Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% END COMPUTATION OF LIKELIHOOD
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



**********************************************************************/
