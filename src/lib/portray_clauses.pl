
:- use_module(library(listing)).      % portray_clause/2,3

/** portray_clauses( +List, +OptS ).

Record a bunch of clauses to either a stream or a file. Supports append and write modes.

OptS can be a list or single option term from the following:

Opts
  * mode(Mode=append)
    prepend, append or write; prepending (for now) reads old terms to memory

  * stream(Stream)
    default is user_output

  * file(File)
    if present, overwrites Stream.

  * write_opts(WOpts=[])
    options to pass to portray_clause/3. when [] is given, portray_clause/2 is used.

==
?- portray_clauses( [a(b,c),b(d,e),c(f,g.t)], [] ).
a(b, c).
b(d, e).
c(f, g.t).
true.

?- File = 'test_prep.pl',
    portray_clauses( [b(3,4),c(5,6)], file(File) ),
    portray_clauses( [a(1,2)], [file(File),mode(prepend)] ),
    atom_concat( 'cat ', File, Cat ),
    shell( Cat ).

a(1, 2).
b(3, 4).
c(5, 6).

==

@author nicos angelopoulos
@version  0.1 2016/12/10
@version  0.2 2018/12/10, removed some private code reference and added WOpts
@version  0.3 2020/04/05, added mode(prepend)

*/

:- lib(en_list/2).

portray_clauses( List, Opt ) :-
    en_list( Opt, Opts ),
    ( memberchk(file(File),Opts) ->
        ( memberchk(mode(ModePrv),Opts) -> 
            ( ModePrv = prepend ->
                Mode = write,
                portray_clauses_read_olds( File, Olds )
                ;
                Mode = ModePrv
            )
            ; Mode = write ),
        open( File, Mode, Out )
        ;
        ( memberchk(stream(Out),Opts) -> true; Out= user_output )
    ),
    ( memberchk(write_opts(WOpts), Opts) -> true; WOpts = [quoted(true)] ),
    portray_clauses_opts( WOpts, List, Out ),
    ( var(File) -> true
        ;
        close( Out ),
        ( ModePrv == prepend ->
            portray_clauses( Olds, [file(File),mode(append)]  )
            ;
            true
        )
    ).

portray_clauses_opts( [], List, Out ) :- !,
    portray_clauses_onto( List, Out ).
portray_clauses_opts( WOpts, List, Out ) :-
    portray_clauses_onto_opts( List, Out, WOpts ).

portray_clauses_onto( [], _Out ).
portray_clauses_onto( [H|T], Out ) :-
    portray_clause( Out, H ),
    portray_clauses_onto_opts( T, Out ).

portray_clauses_onto_opts( [], _, _WOpts ).
portray_clauses_onto_opts( [H|T], Out, WOpts ) :-
    portray_clause( Out, H, WOpts ),
    portray_clauses_onto_opts( T, Out, WOpts ).

portray_clauses_read_olds( File, Olds ) :-
    open( File, read, In ),
    read_term( In, Term, [] ),
    portray_clauses_read_stream_olds( Term, Olds, In ).

portray_clauses_read_stream_olds( end_of_file, Olds, _In ) :-
    !,
    Olds = [].
portray_clauses_read_stream_olds( Term, [Term|Terms], In ) :-
    read_term( In, Next, [] ),
    portray_clauses_read_stream_olds( Next, Terms, In ).
