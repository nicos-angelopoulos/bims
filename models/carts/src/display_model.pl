:- module( bims_display_model_cart, [display_model/2] ).

:- use_module( library(bims) ).
% :- ensure_loaded( '../../src/init_lib' ).		%.
% :- ensure_loaded( bims('models/bns/src/auxil/disp_bn' ).

:- ensure_loaded( library(charsio) ).		% write_to_chars/2
:- ensure_loaded( library(system) ).		% system/1, environ/2.
:- ensure_loaded( library(lists) ).		% append/3, memberchk/2.
:- lib(unique_filename/3).
:- lib(delete_file_if/2).

/* 
     Model = cart( f1, 1, leaf(_,[]), leaf(_,[]) ),
     display_model( Model, [notitle(true)] )
     */
display_model( Cart, Opts ) :-
	( bims:bims_bb_delete( unique_cart_bid, _ ) -> true ; true ),
	% bb_put( node_width, 0.4 ),
	display_model_defaults( Defs ),
	append( Opts, Defs, CmpOpts ),
	kats_n_zeros( Kats, Zeros ),
	cart_to_structure( Cart, 1, Kats/Zeros, _, _, Str ),
     % mktemp( tmp_disp_XXXXXX, PrvTmp ),
     bims:unique_filename( tmp_disp, Tmp, [report(false),extension('')] ),
	memberchk( title(PrvTitle), CmpOpts ),
	memberchk( lhood(PrvLlhood), CmpOpts ),
	memberchk( node_style(NdStl), CmpOpts ),
	memberchk( balanced(BalBool), CmpOpts ),
	memberchk( colour(Clr), CmpOpts ),
	memberchk( node_label(NdLbl), CmpOpts ),
     % ( memberchk(out_was(Tmp),Opts)-> true; true ),
	( memberchk(notitle(true),CmpOpts) ->
          Llhood is 0, Title = []
          ;
          Llhood = PrvLlhood, Title = PrvTitle
     ),
	cart_str_to_disp_dag( Str, Llhood, Title, NdStl, NdLbl, Clr, BalBool, Tmp ),
	memberchk( exec(Exec), CmpOpts ),
	memberchk( trail(Trail), CmpOpts ),
     display_model_exec_def_ext( Exec, DefExt ),
     atom_concat( '.', DefExt, Ext ),
	( memberchk(out(Out),CmpOpts) -> 
		( var(Out) -> 
			atom_concat( Tmp, Ext, Out ),
               OutExt = Out
			;
               ( atom_concat( _, Ext, Out ) ->
                    OutExt = Out
                    ;
                    atom_concat( Out, Ext, OutExt )
               )
		)
		;
		atom_concat( Tmp, Ext, Out ),
          OutExt = Out
	),
     % write( outext(OutExt) ), nl,
	( Exec = gv -> 
		atomic_list_concat( ['dot -Tps ', Tmp, ' -o ', OutExt], DotPs ),
		system( DotPs ),
		atomic_list_concat( [Exec,' ',OutExt,' ', Trail], Display )
		;
		% set changable extension with can change default value
		% depending on exec/1. Allow handle for out/1 file.
		( (Exec=xv; Exec=eog) -> 
			atomic_list_concat( ['dot -Tpng ', Tmp, ' -o ', OutExt ], DotPs ),
			system( DotPs ),
			atomic_list_concat( [Exec,' ',OutExt, Trail], Display )
			;
			( Exec = eps ->
				atomic_list_concat(['dot -Tps ',Tmp,' -o ',OutExt,' ',Trail],Display)
				;
				atomic_list_concat( [Exec,' ',Tmp,' ',Trail], Display )
			)
		)
	),
     ( memberchk(run(false),Opts) -> true ; shell(Display) ),
	( bims:bims_bb_get(not_first_display_model,true) ->
		sleep(1) ; (sleep(2),bims:bims_bb_put(not_first_display_model,true)) ),
	memberchk( delete(DelBool), CmpOpts ),
	debug( display_model, 'Delete bool: ~w, for file: ~p', [DelBool,Tmp] ),
	( DelBool == true ->
		delete_file_if( Tmp )
		;
		true
	).

display_model_exec_def_ext( eps, 'eps' ) :- !.
display_model_exec_def_ext( gv, 'ps' ) :- !.
display_model_exec_def_ext( xv, 'png' ) :- !.
display_model_exec_def_ext( eog, 'png' ) :- !.
display_model_exec_def_ext( _, 'dot' ) :- !.

display_model_defaults( Defs ) :-
	Defs = [	delete(true), 
			% balanced(true),
			balanced(false),
			colour(colour), % alt. bnw
			exec(Exec),
			lhood('NG'),
			node_label(NdLbl),
			node_style(filled), % alt. empty
			title(cart),
			trail(' & ') ],
	/*
	( (environ('USER','nicos'),environ('HOSTNAME',Hname),
		atom_codes(Hname,HnameCs),\+ append("pc",_,HnameCs) ) ->
		% my laptop doesnt like doted right now.
		Exec = dotty
		;
		% Exec = dotty
		Exec = doted
	),
	*/
	Exec = dotty,
     ( current_predicate(feature_name/2) ->
          NdLbl = id_and_feature_name
          ;
          NdLbl = id_and_feature
     ).
			
kats_n_zeros( Kats, Zeros ) :-
	( current_predicate(data:has_category/2) ->
		% bims:bb_get( all_categories, Kats ),
		data:all_categories(Kats),
		mold_list( Kats, 0, Zeros )
		;
		( bims:bims_bb_get(classes_file,ClassesFile) ->
			open( ClassesFile, read, CFIn ),
			read( CFIn, classes(IdClPs) ),
			close( CFIn ),
			findall( Kat, member(_-Kat,IdClPs), UKats ),
			sort( UKats, Kats ),
			mold_list( Kats, 0, Zeros )
			;
			debug( display_model, 'Found no categories.', [] ),
			Kats = [], Zeros = []
		)
	).

cart_to_structure( cart(F,S,L,R), N, Kats, N:F:S, N3, [L1-N,R1-N,N:F:S|T] ) :-
	N1 is N + 1,
	cart_to_structure( L, N1, Kats, L1, N2, LStr ),
	cart_to_structure( R, N2, Kats, R1, N3, RStr ),
	append( LStr, RStr, T ).
cart_to_structure( leaf(_SD,SIds), N, Kats/Zeros, N:Pop, NxN, [N:Pop] ) :-
	NxN is N + 1,
	( current_predicate(data:has_category/2) ->
		findall( H-HClass, 
			( member(H,SIds ),
			  data:has_category(H,HClass) ),
			  	IdClPs ),
		leaf_ids_classified( IdClPs, Kats, Zeros, Classified ),
		classified_to_slashed( Classified, Pop )
		;
		( bims:bims_bb_get(classes_file,ClassesFile) ->
			open( ClassesFile, read, CFIn ),
			read( CFIn, classes(IdClPs) ),
			close( CFIn ),
			kvs_restrict_sks( SIds, IdClPs, RstrPs ),
			leaf_ids_classified( RstrPs, Kats, Zeros, Classified ),
			classified_to_slashed( Classified, Pop )
			;
			length( SIds, Pop )
		)
	).

mold_list( [], _E, [] ).
mold_list( [_|T], E, [E|R] ) :-
	mold_list( T, E, R ).

leaf_ids_classified( [], _Kats, Classified, Classified ).
leaf_ids_classified( [_H-HClass|T], Kats, Accs, Classified ) :-
	nth1( Nth, Kats, HClass ), 
	nth_with_new( Nth, Accs, Old, New, NxAccs ), 
	New is Old + 1,
	leaf_ids_classified( T, Kats, NxAccs, Classified ).

nth_with_new( 1, [H|T], H, New, [New|T] ).
nth_with_new( Nth, [H|T], Old, New, [H|R] ) :-
	NxNth is Nth - 1,
	nth_with_new( NxNth, T, Old, New, R ).

classified_to_slashed( [F,S|T], F/R ) :-
	!,
	classified_to_slashed( [S|T], R ).
classified_to_slashed( [E], E ).

cart_str_to_disp_dag( Str, Lk, Frq, NdStl, NdLbl, Clr, BBool, Dag ) :-
	pick_parents( Str, Parents, Leaves, Pairs, Edges ),
	keysort( Pairs, Sorted ),
	edges_parents_to_dag( Parents, Leaves, Sorted, Edges, NdStl, NdLbl, Clr, BBool, Lk, Frq, Dag ).

% edges_parents_to_dag( [Left,Right|T], Parents, Dag ) :-
edges_parents_to_dag( Parents, Leaves, Pairs, Edges, NdStl, NdLbl, Clr, Bal, Lk, Frq, Dag ) :-
	open( Dag, write, S ),
	write( S, 'digraph G {' ), nl( S ),
	write( S, '  node [style=' ), write( S, NdStl ),
	write( S, ']' ), nl( S ),
	write( S, '  node [height=0.5]' ), nl( S ),
	write( S, '  node [width=1]' ), nl( S ),
	write( S, '  node [shape=egg]' ), nl( S ),
	ink_colour( Clr, red, Red ),
	write( S, '  edge [color=' ), write( S, Red ),
	write( S, ']' ), nl( S ),
	% write( S, '  1 [color=red]' ), nl( S ),
	leave_parent_nodes( Pairs, S, NdLbl, Clr ),
	% leave_nodes( Leaves, S ),
	% parent_nodes( Parents, S ),
	edges_w_labels( Edges, Parents, Leaves, Bal, S ),

     ( (\+ number(Lk); Lk =:= 0) -> 
          (Frq == [] -> NoTitle=true; write( S, '"' ))
          ;
	     write( S, '"' ),
          write( S, Lk ), write( S, ' \\n ' )
     ),
	ink_colour( Clr, lemonchiffon2, LmnChf2 ),
     ( NoTitle == true ->
          true
          ;
          ( atomic(Frq) ->
	          write( S, Frq ) 
               ;
               write_title_lines( Frq, S )
          ),
          write( S, '"[color=' ),
	     write( S, LmnChf2 ), write( S, ' shape=invhouse]' ), nl( S )
     ),

	write( S, '}' ),
	close( S ).

write_title_lines( [], _S ).
write_title_lines( [H|T], S ) :-
     write( S, H ), write( S, '\\n ' ), 
     write_title_lines( T, S ).

ink_colour( Clr, ClrInk, Ink ) :-
	( Clr == colour -> 
		Ink = ClrInk
		;
		Ink = black
	).

/*
bkg_colour( Clr, ClrBkg, Bkg ) :-
	( Clr == colour -> 
		Bkg = ClrBkg
		;
		Bkg = white
	).
	*/

leave_parent_nodes( [], _S, _NdLbl, _Clr ).
leave_parent_nodes( [Id-Node-F-_Val|T], S, NdLbl, Clr ) :-
	( Id == Node ->
		dbl_quote_protect_pl_term( Node, QNode ),
		write( S, '  ' ), write( S, QNode ), 
		ink_colour( Clr, green, Green ),
		write( S, ' [color=' ), write( S, Green ),
		on_node_label_opt_write( NdLbl, Node, Id+F, S ),
		write( S, ', shape=rectangle]' ), nl( S )
		;
		( Id =:= 1 ->
			ink_colour( Clr, red, Red ),
			write( S, '  ' ), write( S, Node ),
			write( S, ' [color=' ), write( S, Red ),
			on_node_label_opt_write( NdLbl, Node, Id+F, S ),
			write( S, ']' ), nl( S )
			;
			ink_colour( Clr, orange, Orange ),
			write( S, '  ' ), write( S, Node ), 
			write( S, ' [color=' ), write( S, Orange ), 
			on_node_label_opt_write( NdLbl, Node, Id+F, S ),
			write( S, ']' ), nl( S )
		)
	),
	leave_parent_nodes( T, S, NdLbl, Clr ).

on_node_label_opt_write( id_and_feature, _Node, _F, _S ) :-
	!.
on_node_label_opt_write( id_and_feature_name, Node, F, S ) :-
     % write( node(Node)-f(F) ), nl,
	( Node =  _Num:_Cntn -> 
          true
          ; 
	     write( S, ' label= ' ),
          F = Num+VarNum,
          feature_name( VarNum, VarName ),
	     dbl_quote_protect_pl_term( Num:VarName, QLbl ),
	     write( S, QLbl )
     ).
% I think the following was a quick fix. 
% would be cleaner to allow the original Number:Feature term to sip through
% to here. (See cart_parent/2.)
on_node_label_opt_write( excl_id, Node, _F, S ) :-
	!,
	write( S, ' label= ' ),
	( Node =  _Num:Cntn ->
		dbl_quote_protect_pl_term( Cntn, QCntn ),
		write( S, QCntn )
		;
		( (atomic(Node),atom_codes(Node,NodeCs),append(_,[0':|ExsvOfIdCs],NodeCs)) ->
			atom_codes( ExsvOfId, [0'"|ExsvOfIdCs] ),
			write( S, ExsvOfId )
			;
			write( user_error, unrecognised_node_str(Node) ), nl( user_error),
			% dbl_quote_protect_pl_term( Node, QNode ),
			write( S, Node )
		)
	).
on_node_label_opt_write( L, _Node, Id+F, S ) :-
	is_list( L ),
	!,
	( F = _:_/_ -> 
		true % by default this label will be present
		;
		( memberchk(F-Lbl,L) ->
			write( S, ' label= ' ),
			dbl_quote_protect_pl_term( Id:Lbl, QLbl ),
			write( S, QLbl )
			;
			throw( id_not_in_labels_list(F,L) )
		)
	).
on_node_label_opt_write( Oth, _Node, _F, _S ) :-
	write( user_error, skipping_nrecognised_node_lbl_opt(Oth) ), nl( user_error ).

	/* ?? twice ??
leave_parent_nodes( [], _S, _Clr ).
leave_parent_nodes( [Id-Node-_Val|T], S, Clr ) :-
	( Id == Node ->
		dbl_quote_protect_pl_term( Node, QNode ),
		write( S, '  ' ), write( S, QNode ), 
		ink_colour( Clr, green, Green ),
		write( S, ' [color=' ), write( S, Green ),
		write( S, ', shape=rectangle]' ), nl( S )
		;
		( Id =:= 1 ->
			ink_colour( Clr, red, Red ),
			write( S, '  ' ), write( S, Node ),
			write( S, ' [color=' ), write( S, Red ),
			write( S, ']' ), nl( S )
			;
			ink_colour( Clr, orange, Orange ),
			write( S, '  ' ), write( S, Node ), 
			write( S, ' [color=' ), write( S, Orange ), 
			write( S, ']' ), nl( S )
		)
	),
	leave_parent_nodes( T, S, Clr ).
	*/
	

/* 
parent_nodes( [], _S ).
parent_nodes( [N-LLbl|T], S ) :-
	( N =:= 1 ->
		write( S, '  ' ), write( S, LLbl ),
		write( S, ' [color=red]' ), nl( S )
		;
		write( S, '  ' ), write( S, LLbl ), 
		write( S, ' [color=orange]' ), nl( S )
	),
	parent_nodes( T, S ).

leave_nodes( [], _S ).
leave_nodes( [H|T], S ) :-
	write( S, '  "' ), write( S, H ), 
	write( S, '" [color=green]' ), nl( S ),
	leave_nodes( T, S ).
*/ 

edges_w_labels( [], _Parents, _Leaves, _Bal, _S ).
edges_w_labels( [LCh-Par,RCh-Par|T], Parents, Leaves, Bal, S ) :-
	memberchk( Par-ParLbl-_-Value, Parents ),
	write( S, '  ' ), write( S, ParLbl ),
	% ( nth( NL, Leaves, LCh ) -> true ; NL = LCh ),
	( cart_parent( LCh, _-LCs-_-_ ) -> true ;
		dbl_quote_protect_pl_term( LCh, LCs )
	),
	write( S, ' -> ' ), write( S, LCs ),
	write( S, ' [label="' ), 
	% select( Par-LLbl, Parents, NxParents ),
	% LLbl = 
	write( S, '=< ' ),
	write( S, Value ), write( S, '"];' ), nl( S ),
	balance_gen( Bal, QBalNd, S, ParLbl ),
	write( S, '  ' ), write( S, ParLbl ),
	% HERE ABOVE
	% ( nth( NR, Leaves, RCh ) -> true ; NR = RCh ),
	% ( cart_parent( RCh, _-PrpRCh ) -> true ; PrpRCh = RCh ),
	( cart_parent( RCh, _-RCs-_-_ ) -> true ;
		dbl_quote_protect_pl_term( RCh, RCs )
	),
	write( S, ' -> ' ), write( S, RCs ),
	write( S, ' [label="' ), 
	write( S, ' >' ),
	write( S, Value ),
	write( S, '"];' ), nl( S ),
	% write( S, ';' ), nl( S ),
	write( S, '  {rank=same ' ),
	write( S, LCs ), write( S, ' -> ' ), 
	balance_rank( Bal, QBalNd, S ),
	write( S, RCs ),
	write( S, '[style=invis]}' ),
	nl( S ),
	edges_w_labels( T, Parents, Leaves, Bal, S ).

pick_parents( [], [], [], [], [] ).
pick_parents( [H|T], Par, Leaves, Pairs, Edges ) :-
	( cart_edge(H,HEdge) ->
		Par = TPar,
		Leaves = TLeaves,
		Pairs = TPairs,
		Edges = [HEdge|TEdges]
		;
		( cart_parent(H,ParH) ->
			Par = [ParH|TPar],
			Leaves = TLeaves,
			Pairs = [ParH|TPairs],
			Edges = TEdges
			;
			Par = TPar,
			Leaves = [H|TLeaves],
			Pairs = [H-H-H-H|TPairs],
			Edges = TEdges
		)
	),
	pick_parents( T, TPar, TLeaves,TPairs, TEdges ).

cart_edge( Child-Parent, Child-Parent ).
	% ( cart_parent( Child, _N-EdgeCh ) -> true
		% ; EdgeCh = Child ),
	% ( cart_parent( Parent, _N-EdgePar ) -> true
		% ; EdgePar = Parent ).

cart_parent( N:F:Ch, N-QH-F-Ch ) :-
	% number_codes( N, Ns ), 
	% number_codes( F, Fs ),
	dbl_quote_protect_pl_term( N:F, QH ).
	% append( [0'_|Fs], [0'_|ChCs], SfxCs ),
	% append( Ns, SfxCs, ParHCs ),

dbl_quote_protect_pl_term( Term, Q ) :-
	write_to_chars( Term, TCs ),
	append( [0'"|TCs], [0'"], QCs ),
	atom_codes( Q, QCs ).

% we assume both the first args are sorted, and pairs have all keys.
%
kvs_restrict_sks( [], _, [] ). 
kvs_restrict_sks( [H|T], [K-_V|Tkv], RstrPs ) :-
	K @< H,
	!, % then skip that pair.
	kvs_restrict_sks( [H|T], Tkv, RstrPs ).
kvs_restrict_sks( [_H|T], [K-V|Tkv], [K-V|TRstr] ) :-
	% it is expected that: K == H,
	kvs_restrict_sks( T, Tkv, TRstr ).

balance_gen( true, QBalNd, S, ParLbl ) :-
	!,
	( bims:bims_bb_get(unique_cart_bid,Nth) ->
		NxNth is Nth + 1
		; 
		Nth is 1,
		NxNth is 2
	),
	number_codes( Nth, NCs ),
	append( "ubid_", NCs, BalNdCs ),
	atom_codes( BalNd, BalNdCs ),
	dbl_quote_protect_pl_term( BalNd, QBalNd ),
	write( S, '  ' ), write( S, QBalNd ),
	write( S, ' [label="",style=invis];' ), 
	% write( S, ' [label="",width=1,style=invis];' ), 
	nl( S ),
	write( S, '  ' ), write( S, ParLbl ),
	write( S, ' -> ' ), write( S, QBalNd ),
	write( S, ' [style=invis];' ),
	nl( S ),
	bims:bims_bb_put( unique_cart_bid, NxNth ).
balance_gen( _, _, _, _ ).
balance_rank( true, QBalNd, S ) :-
	!,
	write( S, QBalNd ),
	write( S, ' -> ' ).
balance_rank( _, _, _ ).
