:- ensure_loaded( library(lists) ).		% select/3.

pad_ranges( Pad, XRange, YRange, XRet, YRet ) :-
	pad_range( XRange, Pad, xpad, XPadded ),
	pad_range( YRange, Pad, ypad, YPadded ),
	stretch_null_range( XPadded, XRet ),
	stretch_null_range( YPadded, YRet ).

pad_range( Min/Max, Pad, Whc, NwMin/NwMax ) :-
	Dist is Max - Min,
	LocMin =.. [Whc,min(MinRl)],
	( memberchk(LocMin,Pad) -> 
		apply_pad_range_rule_to_endpoint( MinRl, Dist, Min, -, NwMin )
		;
		( memberchk(pad(min(MinRl)),Pad) ->
			apply_pad_range_rule_to_endpoint( MinRl, Dist, Min, -, NwMin )
			;
			NwMin = Min
		)
	),
	LocMax =.. [Whc,max(MaxRl)],
	( memberchk(LocMax,Pad) -> 
		apply_pad_range_rule_to_endpoint( MaxRl, Dist, Max, +, NwMax )
		;
		( memberchk(pad(max(MaxRl)),Pad) ->
			apply_pad_range_rule_to_endpoint( MaxRl, Dist, Max, +, NwMax )
			;
			NwMax = Max
		)
	).

apply_pad_range_rules( [], Padded, Padded ).
apply_pad_range_rules( [H|T], Range, Padded ) :-
	once( apply_pad_range_rule(H,Range,NxRange) ),
	apply_pad_range_rules( T, NxRange, Padded ).

apply_pad_range_rule( both(WhatWhat), Min/Max, NwMin/NwMax ) :-
	Dist is Max - Min,
	apply_pad_range_rule_to_endpoint( WhatWhat, Dist, Min, -, NwMin ),
	apply_pad_range_rule_to_endpoint( WhatWhat, Dist, Max, +, NwMax ).
apply_pad_range_rule( max(WhatWhat), Min/Max, Min/NwMax ) :-
	Dist is Max - Min,
	apply_pad_range_rule_to_endpoint( WhatWhat, Dist, Max, +, NwMax ).
apply_pad_range_rule( min(WhatWhat), Min/Max, NwMin/Max ) :-
	Dist is Max - Min,
	apply_pad_range_rule_to_endpoint( WhatWhat, Dist, Min, -, NwMin ).

apply_pad_range_rule_to_endpoint( pcn(Amount), Dist, Point, Sign, NwPoint ) :-
	Margin is Dist * (Amount/100),
	Exp =.. [Sign,Point,Margin],
	NwPoint is Exp.
apply_pad_range_rule_to_endpoint( '%'(Amount), Dist, Point, Sign, NwPoint ) :-
	Margin is Dist * (Amount/100),
	Exp =.. [Sign,Point,Margin],
	NwPoint is Exp.
apply_pad_range_rule_to_endpoint( abs(NwPoint), _Dist, _Point, _Sign, NwPoint ).
apply_pad_range_rule_to_endpoint( mgn(Margin), _Dist, Point, Sign, NwPoint ) :-
	Exp =.. [Sign,Point,Margin],
	NwPoint is Exp.

stretch_null_range( Point/Point, Low/High ) :-
	!,
	Pad is Point / 10,
	Low is Point - Pad,
	High is Point + Pad.
stretch_null_range( Any, Any ).
