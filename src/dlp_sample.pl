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

Uniform selection of a list member:
==
?- dlp_sload(umember).

?- dlp_seed.
?- dlp_sample(umember([a,b,c,d],X) ).
X = d.
==

Assuming packs, mlu, b_real and Real are installed, then plots can be created with sampling outputs
==
?- dlp_load(umember).
?- lib(mlu)
?- mlu_sample( dlp_sample(umember([a,b,c,d,e,f,g,h],X)), 1000, X, Freqs ),
   mlu_frequency_plot( Freqs, [interface(barplot),outputs(svg),las = 2]).
==
Produces file: real_plot.svg

[[html/images/real_plot.svg]]

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
