/** dlp_call( +Goal ).
    dlp_call( +Goal, -Path, -Prb ).

Refute a distributional goal from the clauses in memory (module dlp_sld) using standard SLD resolution.

Succeeds for all possible derivations of Goal.

==
?- dlp_load(coin).
?- dlp_seed.

?- dlp_call(coin(Flip)).
Flip = head ;
Flip = tail ;
false.

?- dlp_call(coin(Flip), Path, Prb).
Flip = head,
Path = [1/0.5],
Prb = 0.5 ;
Flip = tail,
Path = [2/0.5],
Prb = 0.5 ;
false.

==

@author nicos angelopoulos
@version  0:1 2023/05/07

*/
dlp_call( Goal ) :-
     Goal =.. [Functor|Args],
     length( Args, Arity ),
     spec_constructs_scall( Functor/Arity, _Type, _Path, Args, Slp ), 
     dlp_sld:Slp.
     
dlp_call( Goal, Path, Prb ) :-
     Goal =.. [Functor|Args],
     length( Args, Arity ),
     spec_constructs_scall( Functor/Arity, _Type, Path, Args, Slp ), 
     dlp_sld:Slp,
     dlp_path_prob( Path, 1, Prb ).
          
/** dlp_call_sum( +Goal, -Prob ).

Prob is the sum of probabilities for all refutations of Goal- which should be a distributional goal.

Standard SLD resolution is used to derive all refutations.

==
?- dlp_load(doubles).
?- dlp_call_sum(coin(Flip), Prb).
Prb = 1.0.

?- dlp_call_sum(coin(head), Prb).
Prb = 0.5.

?- dlp_call_sum(doubles(head), Prb).
Prb = 0.25.

?- dlp_call_sum(doubles(_), Prb).
Prb = 0.5.
==

A more interesting example
==
?- dlp_load(umember).

?- dlp_call_sum( umember([a,b,c,d],X), Prb ).
Prb = 1.0.

?- dlp_call_sum( umember([a,b,c,d],a), Prb ).
Prb = 0.25.

?- dlp_call_sum( umember([a,b,c,d],b), Prb ).
Prb = 0.25.

?- dlp_call_sum( umember([a,b,c,d],c), Prb ).
Prb = 0.25.

?- dlp_call_sum( umember([a,b,c,d],d), Prb ).
Prb = 0.25.
==

@author nicos angelopoulos
@version  0:1 2023/05/07

*/
dlp_call_sum( Goal, Prob ) :-
     findall( Brb, dlp_call(Goal,_Path,Brb), Brbs ),
     sum_list( Brbs, Prob ).
