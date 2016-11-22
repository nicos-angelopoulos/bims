
mcmcms_message_prefix( error, '! ' ).
mcmcms_message_prefix( warning, '# ' ).

:- requires( to_functor/2 ).

:- multifile( message_hook/3 ).
:- dynamic( message_hook/3 ).

/*  test
testo :- throw( error(existance_error(environ,malaka,somewhere/23)) ).
teste :- print_message( error, error(existance_error(environ,malaka,somewhere/2)) ).
testi :- print_message( warning, error(existance_error(environ,malaka,somewhere/2)) ).
*/

/* This is the test case, i should be using this mechanism more */
user:message_hook(error(existance_error(environ,Var,Goal)),
                                                 Type,_InLines) :-
               mcmcms_message_prefix( Type, Pfx ),
               to_functor( Goal, Fun ),
               Lines = [
                  '~w: Required environment variable `~w\' doesnot exist '-[Fun,Var]],
               print_message_lines(user_error, Pfx, Lines ).

/* we need this  for the throw cases...  in Swi */
user:message_hook(unhandled_exception(Exc),Type,_Mess) :-
     ( message_hook(Exc,Type,[]) ->
          true
          ;
          fail
     ).

% user:message_hook(A,B,C):-write(found(a(A),b(B),c(C))), nl.
% :- assert( user:(message_hook(A,B,C):-write(found(a(A),b(B),c(C))), nl ) ).
