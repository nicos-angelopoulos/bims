
:- ensure_loaded( library(lists) ).          % memberchk/2.

%% option_in_values_or_else_default( Name, Opts, Rep, Vals, Defs, Value )
% Check that Name option's value if exists in Opts is one of Vals.
%

option_in_values_or_else_default( Name, Opts, Vals, Rep, Defs, Value ) :-
     functor( Opt, Name, 1 ),
     arg( 1, Opt, OptVal ),
     Here = option_in_values_or_else_default/6,
     ( memberchk(Opt,Opts) ->
          option_in_values_check( OptVal, Opt, Vals, Here, Rep, Value )
          ;
          ( memberchk(Opt,Defs) ->
               option_in_values_check( OptVal, Opt, Vals, Here, Rep, Value )
               ;
               throw( error(existence_error(Name/1, opts(Opts)-defs(Defs)),Here) )
          )
     ).

option_in_values_check( OptVal, Opt, Values, Here, Rep, Value ) :-
     ( memberchk(OptVal,Values) ->
          OptVal = Value
          ;
          Expected = [error,warning,fail,true],
          ( var(Rep) ->
               throw( error(type_error(Expected, arg(Here,4,var(Rep))),Here) )
               ;
               ( memberchk(Rep,Expected) ->
                    report_option_not_in_values( Rep, Opt, Values, Here )
                    ;
                    throw( error(type_error(Expected, arg(Here,4,Rep)),Here) )
               )
          )
     ).

report_option_not_in_values( error, Term, Vals, There ) :-
     throw( error(type_error(arg_in(Vals), Term),There) ).
report_option_not_in_values( warning, Term, Vals, There ) :-
     print_message( warning(type_error(arg_in(Vals),Term),There) ).
report_option_not_in_values( fail, Term, Vals, There ) :-
     print_message( warning(type_error(arg_in(Vals),Term),There) ),
     fail.
report_option_not_in_values( true, _Term, _Vals, _There ).

/*
testo_1 :- 
     option_in_values_or_else_default( mamu, [mamu(nako)], [naku,nako], error, [mamu(naku)], V ), 
     write( value(V) ), nl.
testo_2 :- 
     option_in_values_or_else_default( mamu, [], [naku], error, [mamu(naku)], V ), 
     write( value(V) ), nl.
testo_3 :- 
     option_in_values_or_else_default( mamu, [], [nacu], error, [mamu(naku)], V ), 
     write( value(V) ), nl.
testo_4 :- 
     option_in_values_or_else_default( mamu, [], [nacu], malaka, [mamu(naku)], V ), 
     write( value(V) ), nl.
testo_5 :- 
     option_in_values_or_else_default( manu, [], [nacu], malaka, [mamu(naku)], V ), 
     write( value(V) ), nl.
testo_6 :- 
     option_in_values_or_else_default( mamu, [], [nacu], _Malaka, [mamu(naku)], V ), 
     write( value(V) ), nl.
     */
