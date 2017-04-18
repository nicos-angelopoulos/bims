---++ Introduction
Bims (Bayesian inference over model structures) implements MCMC learning
over statistical models defined in the Dlp (Distributional logic programming)
probabilistic language. 

Bims is released under the MIT licence.

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
that sub-directory data/. Copy your data there and pass options data/1 to the basename
of the data file. 

For example, assuming your data are in data/bcw.pl
==
?- bims( [models(carts),data(bcw)] ).
==

---+++ Learning new statistical models.

To learn new statistical models create a subdirectory within models/ with new model name
and place within there directories lklhoods and dlps containing code implementing the 
likelihood and prior progrms respectively.

See models/carts and models/bns for examples.

---++ Pack info

@author Nicos Angelopoulos, http://stoics.org.uk/~nicos
@author James Cussens (University of York), http://cs.york.ac.uk/~jc
@version  2.2.0 2017/4/18
@see http://stoics.org.uk/~nicos/sware/bims
@tbd bims_default(-Def).
@tbd test on Windows (and Mac ?)
@license  MIT
