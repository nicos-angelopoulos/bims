
/** dlp_seed.

Set random seed to a standard location.

A convenience predicate for running the examples from a common starting point for the random seed.

Specifically it unfolds to
==
?- set_random(seed(101)).
==

==
?- dlp_load(coin).
?- dlp_seed.
?- dlp_sample(coin(Flip)).
Flip = head.

?- set_random(seed(101)).
?- dlp_sample(coin(Flip)).
Flip = head.
==

@author nicos angelopoulos
@version  0.1 2023/05/07

*/

dlp_seed :-
     set_random(seed(101)).
