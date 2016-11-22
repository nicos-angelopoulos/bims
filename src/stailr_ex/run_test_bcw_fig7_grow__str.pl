%%% Test tail recursive optimization.
%%%
% ------------
% this is a run in the spirit of Fig. 7 (RHS) of the paper.
% It uses a much faster, prior program than the one reported in print.
% Be warned to change Dir to some filesystem with loads of free space.
% To reproduce the likelihood trajectory file for the experiment, do:
% (a)   run the experiment by, starting a prolog (SICStus or Yap) and
%       compile( bcw_fig7_grow ).
%       run_bcw_fig7_grow.
%       halt.
% (b)   cd into out_bcw_fig7_grow or directory you have chosen and 
%       gunzip tr_uc_rm_idsd_a0_95b1_i60K__s1.gz
% (c)   start a prolog and
%       ['../../../carts/auxil/lld_stats_for_stem'].
%       lld_stats_for_stem( [tr_uc_rm_idsd_a0_95b1_i60K__s1] ).
%       halt.
% (d)   gzip tr_uc_rm_idsd_a0_95b1_i60K__s1
%
% The likelihoods for the models in the chain are in tr_uc_rm_idsd_a0_95b1_i60K__s1.llhoods
% Use gnuplot manually or via mcmcms/auxil/gsplay.pl to greate a .eps file from
% thsse numbers.


:- ensure_loaded( '../../run' ).
:- elm( 'carts/src/lhood' ).            % llhood_w_diff/5, model_llhood/2.
:- elm( 'auxil/create_mvisits' ).       % exec_extension/2.

run_test_bcw_fig7_grow__str :-
	Dir = '/local/d1p1/nicos/kong/ijcai05/bcw/rr_test_bcw_fig7_grow__str',
	% Dir = out_bcw_fig7_grow,
	Runs = [		runs_file( runs_test_bcw_fig7_grow__str ),
				results_dir( Dir ),
				print([24,28,29,31])
		],
	run( Runs ).
