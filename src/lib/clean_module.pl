clean_module( Mod ) :-
	% this is only available on Sictus, for other systems
	% make sure  the module  is clean.
	debug( bims, 'Cleaning module: ~w', [Mod] ),
	pl(sicstus(_Sicstus), (assert( (Mod:dummy) ), abolish( Mod:_)) ),
	pl(yap(_Yap), abolish_module(Mod) ),
	pl(swi(_Swi), 
		( ( predicate_property(Head,imported_from(Mod))
		    ; predicate_property(Mod:Head,interpreted)
		  ),
		  functor( Head, Functor, Arity ),
		  abolish(Mod:Functor/Arity), fail ; true )
	).
	% pl(yap(_Yap), abolish(slp:_)).
