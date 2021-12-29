
:- lib(en_list/2).

werr( MessIn ) :-
     % default call is `true'
     en_list( MessIn, MessList ),
     werr( MessList, '', '', user_error ).

werr( MessIn, Prefix ) :-
     en_list( MessIn, MessList ),
     werr( MessList, Prefix, '', user_error ).

werr( MessIn, Prefix, Sep ) :-
     en_list( MessIn, MessList ),
     werr( MessList, Prefix, Sep, user_error ).

werrc( MessIn, Call ) :-
     en_list( MessIn, MessList ),
     werrc( MessList, '', Call ).

werrc( MessIn, Prefix, Call ) :-
     en_list( MessIn, MessList ),
     werr( MessList, Prefix, '', user_error ),
     call( Call ).

werrc( MessIn, Prefix, Stream, Call ) :-
     en_list( MessIn, MessList ),
     werr( MessList, Prefix, '', Stream ),
     call( Call ).

werrh( MessIn ) :-
     werrc( MessIn, error, halt(1) ).
werrh( MessIn, Code ) :-
     werrc( MessIn, error, halt(Code) ).

werr( [], _Pfx, _Sep, _S ).
werr( [H|T], PfxPrv, Sep, S ) :-
     ( werr_prefix(PfxPrv,Pfx) -> true; Pfx=PfxPrv),
     ( (H = [_|_];H = []) ->
          write( S, Pfx ),
          werr_2( H, Sep, S ),
          R = T
          ;
          write( S, Pfx ),
          werr_2( [H|T], Sep, S ),
          R = []
     ),
     werr( R, Pfx, Sep, S ).

werr_2( [], _Sep, S ) :-
     nl( S ).
werr_2( [H|T], Sep, S ) :-

     ToAtom = atom_codes( AtH, H ),
     ( (H = [_|_],catch(ToAtom,_,fail)) ->
          true
          ;
          ( H = l(AtH) ->  % actually a list, ...
               true
               ;
               AtH = H
          )
     ),
     write( S, AtH ),
     ( T == [] -> true; write( S, Sep )),
     werr_2( T, Sep, S ).

werr_prefix( error, '? ' ).
werr_prefix( warning, '! ' ).
werr_prefix( warn, '! ' ).
werr_prefix( info, '% ' ).
