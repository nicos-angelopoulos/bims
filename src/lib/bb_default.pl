%% bb_default( +Key, +Default ).
%
% Only set bb_put( Key, Default ) if bb_get( Key, _ ) fails.
%
bb_default( Key, _Def ) :-
	bims_bb_get( Key, _ ),
	!.
bb_default( Key, Def ) :-
	bims_bb_put( Key, Def ).
