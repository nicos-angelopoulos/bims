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

The default likelihood is taken from: fixme: 

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

The default likelihood is an implementation of the classification likelihood function presented in: 
H Chipman, E George, and R McCulloch. Bayesian CART model search (with
discussion). J. of the American Statistical Association, 93:935–960, 1998.

---+++ Learning models from new datasets

An easy way to run Bims on your data is to create a new directory and within 
that sub-directory data/ copy your data there and pass options data/1 to the basename
of the data file. 

For example, 
==
?- 
==

---+++ Learning new statistical models.

@author Nicos Angelopoulos, http://stoics.org.uk/~nicos
@author James Cussens (University of York), http://cs.york.ac.uk/~jc
@version  1.1.0 2016/1/29
@see http://stoics.org.uk/~nicos/sware/bims
@tbd bims_default(-Def).
@tbd test on Windows (and Mac ?)
@license  GPL 2 or Artistic 2.0
