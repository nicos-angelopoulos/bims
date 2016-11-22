:- requires( to_list/2 ).

%% report( +Trigger, +This ).
%
%  Print This to stream stored in bims_report:stream/1 if Trigger is a printable trigger in the
%  current run.
report( Trigger, This ) :-
	bims_report:report(Trigger),
	!,
	bims_report:stream( Stream ),
	portray_clause( Stream, This ).
report( _Trigger, _This ).
