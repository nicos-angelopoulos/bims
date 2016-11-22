:- pl( swi(_), ensure_loaded(library(swi_compat)),
               ensure_loaded(library(terms)) ).
% note: swi doesnt have terms lib. implement (at least variant).

% list_frequency( List, FrequenciesOfList ) :-
% FrequenciesOfList is a list of Terms-Freq Pairs
% of how many times each term (and its variants)
% appear in the List.

list_frequency( [H|T], [H-HTimes|CountedT] ) :-
        list_frequency_1( T, H, 1, HTimes, ReducedT ),
        list_frequency( ReducedT, CountedT ).
list_frequency( [], [] ).

list_frequency_1( [H|T], El, Acc, Count, RedT ) :-
        variant( El, H ),
        !,
        Acc1 is Acc + 1,
        list_frequency_1( T, El, Acc1, Count, RedT ).
list_frequency_1( [H|T], El, Acc, Count, [H|RedT] ) :-
        list_frequency_1( T, El, Acc, Count, RedT ).
list_frequency_1( [], _El, Acc, Acc, [] ).
