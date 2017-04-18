% Tested on: 
% devel:swi(5:9:11)
% stable-git: yap(5:1:4)
% devel:yap(6:0:5)
%

:- dynamic prolog_system_recogniser/2.
% :- ensure_loaded( library(lists) ).

prolog_system_recogniser( Flag, sicstus(Major:Minor:Fix) ) :-
   atom_concat('SICStus ',VerPfxAtom,Flag),
   atom_concat(VerAtom,_Rem,VerPfxAtom),
   atom_concat(VerOnly,' ',VerAtom),
   atom_concat(MjrPfx,MnrFixAtom,VerOnly),
   atom_concat(MajorAtom,'.',MjrPfx),
   atom_concat(MnrPfx,FixAtom,MnrFixAtom),
   atom_concat(MinorAtom,'.',MnrPfx),
   atom_codes(MajorAtom,MajorCs),
   number_codes(Major,MajorCs),
   atom_codes(MinorAtom,MinorCs),
   number_codes(Minor,MinorCs),
   atom_codes(FixAtom,FixCs),
   number_codes(Fix,FixCs).

prolog_system_recogniser( Flag, yap(Major:Minor:Fix) ) :-
   atom_concat('Yap-',VerPfxAtom,Flag),
   atom_concat(MjrPfx,MnrFixAtom,VerPfxAtom),
   atom_concat(MajorAtom,'.',MjrPfx),
   atom_concat(MnrPfx,FixAtom,MnrFixAtom),
   atom_concat(MinorAtom,'.',MnrPfx),
   atom_codes(MajorAtom,MajorCs),
   number_codes(Major,MajorCs),
   atom_codes(MinorAtom,MinorCs),
   number_codes(Minor,MinorCs),
   atom_codes(FixAtom,FixCs),
   number_codes(Fix,FixCs).

prolog_system_recogniser( Flag, yap(Major:Minor:Fix) ) :-
   % 'YAP 6.0.5 (x86_64-linux): Tue May  4 11:12:56 BST 2010'
   % atom_codes( Flag, FlagCs ),
   atom_concat( 'YAP ', Right, Flag ),
   atom_codes( Right, RightCs ),
   append( VersCs, [0' |_], RightCs ), 
   append( MjrCs, [0'.|MnrFixCs], VersCs ), % '
   append( MnrCs, [0'.|FixCs], MnrFixCs ), % '
   number_codes( Major, MjrCs ),
   number_codes( Minor, MnrCs ),
   number_codes( Fix, FixCs ).

prolog_system_recogniser( Flag, swi(Major:Minor:Fix) ) :-
   number(Flag) ->
   Fix   is Flag mod 100,
   Major is Flag // 10000,
   Minor is ((Flag - (Major*10000)) // 100).

:-
	( current_predicate(pl/1) ->
	  true
	  ;
     ( ( current_prolog_flag( version, VersionFlag ), 
         prolog_system_recogniser(VersionFlag,Engine) ) ->
	      assert( pl(Engine) )
         ;
         M='Unrecognised prolog engine in trying to assert if_pl/1.',
	      write( user_error, M ),
	      nl( user_error )
	  )
	).

pl( Prolog, Call ) :-
	( Prolog = [_|_] -> 
		pl_list( Prolog, Call )
		;
	   	pl_list( [Prolog], Call )
	).

pl_list( [], _Call ).
pl_list( [H|T], Call ) :-
     ( atom(H) -> 
          functor(PlH,H,1)
          ;
          PlH = H
     ),
     ( pl(PlH) -> 
             call(Call)
             ;
             true
     ),
	pl_list( T, Call ).

pl( EngineToken, Then, Else ) :-
     ( atom(EngineToken) ->
          functor( Engine, EngineToken, 1 )
          ;
          Engine = EngineToken
     ),
        ( pl(Engine) -> 
                call( Then )
                ;
                call( Else )
        ).
pl_version( Requested, CutoffVer, Before, ThisAndAfter ) :-
	pl( ThisEngine ),
	( ThisEngine =.. [Requested,RunningVer] ->
		( RunningVer @< CutoffVer -> 
			call( Before )
			;
			call( ThisAndAfter )
		)
		; 
		true
	).

:- abolish( prolog_system_recogniser/2 ).
