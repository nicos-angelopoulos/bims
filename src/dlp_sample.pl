/** dlp_sample( +Goal ).
    dlp_sample( +Goal, -Path, -Prb ).

Sample a distributional goal from the clauses in memory (module dlp_ssd) using stochastic resolution.

Succeeds at most once.

Instead of using linear (SLP) clausal selection the predicate using stochastic selection- where
clauses are selected proportionally to the probabilistic values attached to them.
Thus a clause with probability label of =|1/2|= will be selected twice as often as its sister 
clause that has probability label of =|1/4|=.

==
?- dlp_load(coin).
?- dlp_seed.
?- dlp_sample(coin(Flip)).
Flip = head.

?- dlp_sample(coin(Flip)).
Flip = tail.

?- dlp_seed.
?- dlp_sample(coin(Flip),Path,Prb).
Flip = head,
Path = [1/0.5],
Prb = 0.5.

?- dlp_sample(coin(Flip),Path,Prb).
Flip = tail,
Path = [2/0.5],
Prb = 0.5.
==

@author nicos angelopoulos
@version  0:1 2023/05/07

*/
dlp_sample( Goal ) :-
     Goal =.. [Functor|Args],
     length( Args, Arity ),
     spec_constructs_scall( Functor/Arity, _Type, _Path, Args, Slp ), 
     dlp_ssd:Slp,
     !.
     
dlp_sample( Goal, Path, Prb ) :-
     Goal =.. [Functor|Args],
     length( Args, Arity ),
     spec_constructs_scall( Functor/Arity, _Type, Path, Args, Slp ), 
     dlp_ssd:Slp,
     !,
     dlp_path_prob( Path, 1, Prb ).
