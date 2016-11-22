% gamma.pl

% 0 <= x <= inf
% mean = Mean, skew= Alpha
% at low skew, (e.g. x=2), x always near mean
% at skew=20, gamma becomes normal

% only supports integer values X and Alpha
:- ensure_loaded( library(random) ).

gamma(Mean,Alpha,Out) :-
	Beta is Mean/Alpha,
	(Alpha > 20
	-> 	Mean is Alpha * Beta,
		Sd is sqrt(Alpha*Beta*Beta),
		normal(Mean,Sd,Out)
	;	gamma(Alpha,Beta,0,Out)).

gamma(0,_,X,X) :- !.
gamma(Alpha,Beta, In, Gamma) :-
     random( R ),
	Temp is In + ( -1 * Beta * log(1-R)),
	Alpha1 is Alpha - 1,
	gamma(Alpha1,Beta,Temp,Gamma).

% normal.pl
% using ranf to generate a normal distribution
% -inf <= x <= inf, mean=M, standard deviation=S

% if "ranf" already loaded,
% then nothing happens 

% :- ensure_loaded(ranf).
% :- arithmetic_function(normal/2).

normal(M,S,N) :-
	box_muller(M,S,N).

% classical fast method for computing
% normal functions using polar co-ords
% (no- i dont understand it either)
box_muller(M,S,N) :-
	w(W0,X),
	W is sqrt((-2.0 * log(W0))/W0),
	Y1 is X * W,
	N is M + Y1*S.

w(W,X) :-
     random( R1 ),
	X1 is 2.0 * R1 - 1,
     random( R2 ),
	X2 is 2.0 * R2 - 1,
	W0 is X1*X1 + X2*X2,
	% IF -> THEN ; ELSE
	% same as xx :- IF,!, THEN,
	%         xx :- ELSE
	% vars bound in IF not available to ELSE
	% no backtracking within the IF
	% -> ; precendence higher than ,
	(W0  >= 1.0 -> w(W,X) ; W0=W, X = X1).
