/** dlp_path_prob( +Path, -Prb ).
    dlp_path_prob( +Path, +Part, Prb ).

Probability of a stochastic path. 

Part can be a starter value, typically Part is 1.

==
?- dlp_load(coin).
?- dlp_seed,
   dlp_sample(coin(Flip),Path,Prb),
   dlp_path_prob(Path,AgainPrb).
==

@author nicos angelopoulos
@version  0:1 2023/05/07

*/
dlp_path_prob( Path, Prob ) :-
     dlp_path_prob( Path, 1, Prob ).

dlp_path_prob( [], Prob, Prob ).
dlp_path_prob( [H|T], Curr, Prob ) :-
     ( (H:This;H/This) -> Next is Curr * This ; Next is Curr ),
     dlp_path_prob( T, Next, Prob ).
