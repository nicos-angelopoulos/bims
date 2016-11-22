%% bb_remove( +Key, -Value ).
%
% Variaant of bb_delete/2 that does not fail if Key is not a known key.
%
%==
% ?- bb_put( what, v1 ).
% ?- bb_delete( what, V ).
% ?- bb_delete( what, V ).
%==
bims_bb_remove( Key, Value ) :-
	bims_bb_delete( Key, Value ),
	!.
bims_bb_remove( _Key, _Value ).
