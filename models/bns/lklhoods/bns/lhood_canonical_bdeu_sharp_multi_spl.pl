:- ensure_loaded( jc_llhood_ratio_fa_multi_spl ).
:- ensure_loaded( jc_alpha_bdeu_sharp ).

lhood_canonical( MODst, _MODi, LLst, LLi, Ratio ) :-
	llhood_ratio( MODst, LLi, 0, LLst, LogofRatio ),
     % fixme: this has been broken since v2.0 at least? 25.09.26, trying to fix this, double check
	(catch((Ratio is exp( LogofRatio )),_,fail) -> true; (LogofRatio > e -> Ratio is 1 ; Ratio is 0)).
	% ( Dirr > 0 -> Ratio is 1; Ratio is exp(Diff) ).
% llhood_ratio( MODst, MODi, LogofRatio ),

% Strictly speaking, we should provide the LLhood, but as
% far as lhood canonical/5 is aware, then it should be fine.
% model_llhood( Mod, LogLLhood )  :-
	% model_log_llhood( Mod, 0, LogLLhood ).

model_llhood( [], [] ).
model_llhood( [Family|T], [Family+Contrib|TofLgLlkCntrbs] ) :-
	llhood_family_contrib( Family, Contrib ),
	model_llhood( T, TofLgLlkCntrbs ).
