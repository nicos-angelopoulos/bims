:- lib(stoics_lib:en_list/2).

%% report_triggers( +These ).
%
%  Set up print hooks for items of These. Can be a non-listed singleton.
%
report_triggers( Those ) :-
     en_list( Those, AllOfThese ),
     sort( AllOfThese, These ),
     retractall( bims_report:report(_) ),
     report_triggers_assert( These ).

report_triggers_assert( These ) :-
     memberchk( all, These ),
     !,
     report_triggers_assert_all.
report_triggers_assert( These ) :-
     findall( _, ( member(This,These),known_printable(This),
                   assert( bims_report:report(This) )
                 ), _ ).

report_triggers_assert_all :-
     known_reportable_term( This, _, _ ),
     assert( bims_report:report(This) ),
     fail.
report_triggers_assert_all.

known_reportable( This ) :-
     known_reportable_term( This, _Ari, _Excpl ),
     !.
known_reportable( This ) :-
     throw( fixme(ignoring_unknown_printable_with_list_of_possibles(This)) ).

% fixme: add public predicate for interrogating this
known_reportable_term( b_pos, 2, 'Backtrack positions sop/2' ). 
known_reportable_term( lkl_l, 3, 'Likelihood of ith model & path length; ll/2'). 
known_reportable_term( m_star, 2, 'Proposed model; follows rules of reporting M_i; b/1' ).
known_reportable_term( l_star, 4, 'Proposed model likelihood and other info; lp/3' ).
known_reportable_term( path, 2, 'Path presented to backtracking; path/1' ).
known_reportable_term( iter, 1, 'Reports iteration\'s end; done/1' ).
known_reportable_term( ratio, 2, 'Ratio of jump decision; ratio/1' ).
known_reportable_term( alpha, 5, 'Alpha calculation of jump decision; alpha/2' ).
