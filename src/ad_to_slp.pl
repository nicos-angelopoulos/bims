
:- op( 550, yfx, :: ).
:- op( 550, yfx, ## ).

% :- ensure_loaded( init_lib ).           % library_directory/1...
:- ensure_loaded( library(lists) ).       % append/3, memberchk/2, length/2.
:- ensure_loaded( library(system) ).      % /rename_file/2, tmp_file/2.

% :- ensure_loaded( library(cc) ).        % init_cc/0, next_cc/1, bims_bb_get(cc).
:- lib(stoics_lib:en_list/2).
:- lib(cc/0).                      % init_cc/0, next_cc/1, bims_bb_get(cc).
:- lib(unique_filename/2).
:- lib(kv_decompose/3).
:- lib(defined_elsewhere/2).
:- lib(delete_file_if/1).
:- lib(clean_module/1).
:- lib(werr/1).
:- lib(flatten_vs/2).
:- lib(is_letter/1).

:- multifile(last_skips/2).
:- dynamic(last_skips/2).

:- dynamic(bims_stailr:stailr/1).
:- dynamic(bims_stailr:lv_no_bpoint/1). % user defined, leaves no backtrack point.

errhandle(Err)  :- 
	write( errhandle(Err) ), nl.

ad_to_slp( Opts ) :-
	( memberchk(help,Opts); memberchk(h,Opts) ; Opts == help ),
	!,
	write('ad_to_slp/1: transform the read-in clauses of ad module to Prolog clauses' ), nl,
	write('             writing onto a temporary file, which is then loaded to memory.'), nl,
	nl,
	write( 'Options:' ), nl,
	write( '         load/1:  load method, in {(cm),cn}, for compile or consult' ), nl,
	write( '         msd/1:   model space identifier, in {(rm),fm}' ), nl,
	write( '         rm/1:    {(true),false,filename} for deleting temmporary file or moving to filename' ),
	write( '         tmp/1:   the temporary file to use (def:slp_transformed.pl)' ), nl,
	nl, nl.

ad_to_slp( InOpts ) :-
	ad_to_slp_defaults( Defs ),
	append( InOpts, Defs, Opts ), 
	clean_module( slp ),
	init_cc,
	bims_bb_get( ad:all_preds, Ps ),
	memberchk( tmp(Tmp), Opts ),
	delete_file_if( Tmp ),
	memberchk( msd(Msd), Opts ),
	initial_slp_declarations_to_file( Tmp, Msd ),
	preds_to_file( Ps, Tmp ),
     built_ins_to_file( Tmp ),
	memberchk( load(Load), Opts ),
	once( load_method_to_load_pname( Load, LdPname ) ),
	LoadCall =.. [LdPname,Tmp],
	call( LoadCall ),
	% abolish( ad:_ ),
	clean_module( ad ),
	memberchk( rm(Rm), Opts ),
	( Rm == true ->
		delete_file_if( Tmp )
		;
		( Rm == false ->
			true
			;
			rename_file( Tmp, Rm )
		)
	).

ad_to_slp_defaults( Defs ) :-
	Defs = [
			% tmp('slp_transformed.pl'),
			tmp(Tmp),
			msd(rm),
			load(cm),
			rm(true)
		],
     UFOpts = [report(false),add(environ('HOSTNAME'),8)],
     unique_filename( translated__slp, Tmp, UFOpts ).

preds_to_file( [], _File ).
preds_to_file( [Hspec|Theads], File ) :-
     % write( Hspec ), nl,
	Hspec = HName/HArity,
	functor( Hhead, HName, HArity ),
	findall( Hhargs-HMB-PVs, 
		(ad:slp_clause(Hhead,PrvHBody),
		( (is_distributional(Hhead),PrvHBody = (PrvPVs,HBody)) ->
		  de_module_vars_list( PrvPVs, PVs ) ; PVs = _, HBody=PrvHBody ),
		% HERE add PrvPVs into de_module_body
		% use it to match recursive pre-computed variables.
		%
		Hhead=..[_F|Hhargs], de_module_body(HBody,PrvPVs,HMB)),
			Trips ),
	kv_tri_decompose( Trips, Heads, Bodies, PVLs ),
	( is_stochastic(Hhead) ->
		findall( Hl, ad:s_label(Hspec,Hl), HLs ),
		if_stailr( Hspec, Heads ),
		pred_to_s_pred( Hhead, Heads, Bodies, HLs, SPred, Switch ),
		record_spred( SPred, File ),
		atom_concat( 'sidx_', HName, Sidx ),
		record_switch( Switch, Sidx, s, File )
		; 
		( is_distributional(Hhead) -> 
			findall( HLEx-HLVs, ad:d_label(Hspec,HLEx,HLVs), LVs ),
			if_stailr( Hspec, Heads ),
			pred_to_d_pred( Hhead, Hspec, Heads, Bodies, LVs, PVLs, DPred, Switch ),
			% write( pred_to_d_pred( Hhead, Hspec, Heads, Bodies, LVs, DPred ) ), nl,
			record_spred( DPred, File ), % change to sd_pred if it works,
			atom_concat( 'sidx_', HName, Sidx ),
			%%% HERE
			flatten_vs( PVLs, FlatPVLs ),
			record_switch( Switch, Sidx, FlatPVLs, File )
			;
			% Hhead =.. [F|_OHhargs],
			functor( Hhead, F, _ ),
			pred_to_ns_pred( Heads, Bodies, F, NSPred ),
               /* here
               ( F == select_feature ->
                    trace,
			     pred_to_ns_pred( Heads, Bodies, F, NSPred ),
			     portray_clause( pred_to_ns_pred(Heads,Bodies,F,NSPred) ),
                    write( heads(Heads)-bodies(Bodies)-nspred(NSPred) ), nl
                    ;
			     pred_to_ns_pred( Heads, Bodies, F, NSPred )
               ),
               */
			record_preds( File, NSPred, F )
		)
	),
	preds_to_file( Theads, File ).

if_stailr( Hspec, Heads ) :-
	length( Heads, Lgth ),
	bims_bb_get( cc, CC ),
	( bims_stailr:stailr( Hspec ) ->
		( Lgth =:= 2 ->
			NxtCC is CC + 1,
			Lskips = ( last_skips(NxtCC,Id):-!,memberchk(Id,[CC,NxtCC]))
			;
			list_of_n_from_offset( Lgth, CC, [CC|TheySkp] ),
			Lskips = ( last_skips(Lst,Id):-memberchk(Lst,TheySkp),!,
						memberchk(Id,[CC|TheySkp]) )
		),
		assert( Lskips )
		;
		true
	),
	( bims_stailr:lv_no_bpoint(Hspec) -> 
		% write( user_error, lv_no(Hspec) ), nl( user_error ),
		list_of_n_from_offset( Lgth, CC, UncoToSkip ),
		findall( USk, (member(USk,UncoToSkip),assert(last_skips(_,USk))), _ )
		;
		true
	).

list_of_n_from_offset( 1, I, [I] ) :- !.
list_of_n_from_offset( N, I, [I|T] ) :-
	NxN is N - 1,
	NxI is I + 1,
	list_of_n_from_offset( NxN, NxI, T ).

pred_to_ns_pred( [], [], _PredName, [] ).
pred_to_ns_pred( [H|Hs], [B|Bs], PredName, [ExpCl|ECls] ) :-
	% cc( Id ),
	bims_bb_get( cc, Id ),
	append( [Id,[Id|Rest]/Lc], H, ExpList ),
	ExpH =.. [PredName|ExpList],
	( B = true ->
		Rest=Lc, ExpCl = ExpH
		;
		expand_body_calls( B, Rest/Lc, ExpB ),
		ExpCl = (ExpH:- ExpB)
	),
	next_cc( Id ),
	pred_to_ns_pred( Hs, Bs, PredName, ECls ).

pred_to_d_pred( Proto, Spec, Hds, Bds, Lbls, PVLs, (ExpHead:-ExpBody), TheSwitch ) :-
	Proto =.. [PredName|Args],
	ExpHead =.. [PredName,[H|Pin]/Lc,InLAs|Args],
	% cc( CC ),
	bims_bb_get( cc, CC ),
	TheSelect = select_id( Hds, Args, Spec/Lbls/InLAs, CC, H, Sid ),
	% TheSelect = select_id_expr( Hds, Args, Spec, Lbls, InLAs, CC, H, Pin, Sel, Sid, Pmid ),
	switch_selector( Hds, Bds, d, Lbls, PVLs, Args, CC, Sid, TheSwitch ),
	atom_concat( 'sidx_', PredName, SidxName ),
	flatten_vs( PVLs, FlatPVLs ),
	SidxCall =.. [SidxName, Sid, Args, FlatPVLs, Pin, Lc ],
	ExpBody = (TheSelect,SidxCall).
	% ExpBody = (TheSelect,TheSwitch).

pred_to_s_pred( Proto, Hds, Bds, Lbls, (ExpHead:-ExpBody), TheSwitch ) :-
	Proto =.. [PredName|Args],
	ExpHead =.. [PredName,[H|Pin]/Lc|Args],
	% cc( CC ),
	bims_bb_get( cc, CC ),
	TheSelect = select_id( Hds, Args, Lbls, CC, H, Sid ),
	switch_selector( Hds, Bds, s, _, _, Args, CC, Sid, TheSwitch ),
	atom_concat( 'sidx_', PredName, SidxName ),
	SidxCall =.. [SidxName, Sid, Args, Pin, Lc ],
	ExpBody = (TheSelect,SidxCall).
	% ExpBody = (TheSelect,TheSwitch).

initial_slp_declarations_to_file( File, Msd ) :-
	open( File, append, Stream ),
	write( Stream, ':- module( slp, [] ).' ),
	nl( Stream ),
	% write( Stream, ':- ' ),
	% write( Stream, 'consult(library(\'../runtime\')).' ),
	% write( Stream, 'compile(library(\'../runtime_' ),
	% write( Stream, Msd ), write( Stream, '\')).' ),
	% nl( Stream ), 
	once( user:file_search_path(bims,Bims) ),
	directory_file_path( Bims, 'src/runtime', RunTimeD ),
	atomic_list_concat( [runtime,Msd], '_', Bname ),
	directory_file_path( RunTimeD, Bname, AbsRunTime ),
	portray_clause( Stream, :- compile(AbsRunTime) ),
	nl( Stream ),
	pl(yap(_Y), (
		write(Stream,':- source.'), nl(Stream), nl(Stream) )
		),
	close( Stream ).

% switch_selector( [], _Bodies, _Labels, Count, Id, TheSwitch ) :-
switch_selector( [HArgs], [Bd], _Tp, [_-Vs], [Vs], _Args, CC, _Id, Switch ) :-
	!,
	% ( Tp = d -> IBd = (Vs,Bd) ; IBd = Bd ),
	expand_body_calls( Bd, Pin/Lc, ExpBd ),
	Switch = [(CC,HArgs,ExpBd,Pin/Lc)],
	next_cc( CC ).
switch_selector( [HArgs|Thas], [Bd|Bds], _Tp, [_-Vs|TVs], [Vs|TPVs], Args, CC, Id, Switch ) :-
	% ( Tp = d -> IBd = (Vs,Bd) ; IBd = Bd ),
	expand_body_calls( Bd, Pin, ExpBd ),
	Switch = [ (CC,HArgs,ExpBd,Pin) | NestSwitch ],
	% Switch = [ (CC,HArgs,ExpBd,Pin,Pout) | NestSwitch ],
	% Switch = ( (Id=:=CC-> HArgs=Args, ExpBd ; NestSwitch) ),
	NxtCC is CC + 1,
	switch_selector( Thas, Bds, _TpDsh, TVs, TPVs, Args, NxtCC, Id, NestSwitch ).

expand_body_calls( (PVrs ## A), Pathin/Ld, ExpCmpl ) :-
	!,
	% HERE
	% trace,
	ExpCmpl = ( Pathin = [NestIn|Ld], ExpA ),
	% ExpCmpl = ( Pathin = [NestIn|Ld], Pathout = [NestOut|Lp], ExpA ),
	( is_distributional( A ) -> 
		A =.. [Pred|Args],
		en_list( PVrs, PrbVrs ),
		ExtA =.. [Pred,PrbVrs|Args]
		;
		write( user_error, 'inappropriate use of ## within body goal' ),
		nl( user_error ),
		write( user_error, goal(PVrs ## A) ), nl( user_error ), abort
	),
	expand_body_calls( ExtA,  NestIn/[], ExpA ).
expand_body_calls( (A->B;C), Pathin/Ld, ExpIf ) :-
	% i dont think this is working properly.
	!,
	ExpIf = ( ExpA->(ExpB,LcB=Ld);(ExpC,LcC=Ld) ),
	expand_body_calls( A,  Pathin/LcA, ExpA ),
	expand_body_calls( B, LcA/LcB, ExpB ),
	expand_body_calls( C, Pathin/LcC, ExpC ).
expand_body_calls( (A,B), Pathin/Lc, (ExpA,ExpB) ) :-
	!,
	expand_body_calls( A,  Pathin/LcA, ExpA ),
	expand_body_calls( B, LcA/Lc, ExpB ).
expand_body_calls( (\+ PrpA), Pathin/Lc, (\+ ExpA) ) :-
	!,
	meta_ad_strip( PrpA, A ),
	expand_body_calls( A, _Fin/_FLcA, ExpA ),
	Pathin = Lc.
expand_body_calls( findall(A,PrpB,C), Pathin/Lc, ExpF ) :-
	!,
	meta_ad_strip( PrpB, B ),
	expand_body_calls( B,  _Fin/_FLcA, ExpB ),
	Pathin = Lc,
	ExpF = findall(A,ExpB,C).
expand_body_calls( setof(A,PrpB,C), Pathin/Lc, ExpF ) :-
	!,
	meta_ad_strip( PrpB, B ),
	expand_body_calls( B,  _Fin/_FLcA, ExpB ),
	Pathin = Lc,
	ExpF = setof(A,ExpB,C).
expand_body_calls( PrpA, Pathin/Lc, ExpA ) :-
	meta_ad_strip( PrpA, A ),
	( is_stochastic_or_ext_distributional(A) ->
		A =.. [PredName|Args],
		ExpA =.. [PredName,Pathin/Lc|Args]
		;
		( is_distributional(A) ->
			A =.. [PredName|Args], 
			ExpA =.. [PredName,Pathin/Lc,_|Args]
			;
               %%% write( trying(A,Mod) ), nl,
               %% 20080516( A = ord_add_element(_,_,_) -> trace; true ),
			( defined_elsewhere(A,Mod) -> 
				% Pathin = Lc,
				% Pathout = Lp,
				% ExpA = (Mod:A,Pathin = Lc) % GOOD ONE
				ExpA = Mod:A, Pathin = Lc % ATTEMPTING optimisation
				% ExpA = (Mod:A, Pathout = Pathin)
				;
				( is_non_stochastic(A) ->
					A =.. [PredName|Args],
					append( [PredName,_,Pathin/Lc], Args, ExpAList ),
					ExpA =.. ExpAList
					;
					ExpA = user:A % assume it will be there...
				)
			)
		)
	).

spec_of_type( Spec, Type ) :-
	ad_pt:pred_type( Spec, Type ),
	!.

is_stochastic_or_ext_distributional( A ) :-
	goal_spec( A, Spec ), 
	( spec_of_type( Spec, s ) -> 
		true 
		; 
		Spec = Name/Arity,
		RArity is Arity - 1,
		spec_of_type( Name/RArity, d )
	),
	!.

is_stochastic( Head ) :-
	goal_spec( Head, Spec ),
	spec_of_type(Spec, s).
is_distributional( Head ) :-
	goal_spec( Head, Spec ),
	spec_of_type(Spec, d).
is_non_stochastic( Head ) :-
	goal_spec( Head, Spec ),
	spec_of_type(Spec, ns).
% make the following flag the presence of s_random,
% so code only need to be added to the transformed slp if some body included this.
% see built_in addition of s_random in this file
%
is_non_stochastic( s_random(_) ).

record_spred( SPred, File ) :-
	open( File, append, Stream ), 
	current_output( Output ),
	set_output( Stream ),
	portray_clause( SPred ), nl,
	set_output( Output ),
	close( Stream ).

record_preds( File, Pairs, PredName ) :-
	open( File, append, Stream ), 
	current_output( Output ),
	set_output( Stream ),
	record_ns_clauses( Pairs, PredName ),
	set_output( Output ),
	close( Stream ).

record_ns_clauses( [], _PredName ).
record_ns_clauses( [Clause|T], PredName ) :-
	portray_clause( Clause ), nl,
	record_ns_clauses( T, PredName ).

record_switch( List, Name, VsOr, File ) :-
	open( File, append, Out ),
	current_output( Cur ),
	set_output( Out ), 
	record_switch_1( List, VsOr, Name ),
	set_output( Cur ),
	close( Out ).

record_switch_1( [], _, _ ) :- nl.
record_switch_1( [H|T], VsOr, PName ) :-
	H = (Id,Args,Body,Pin/Lc),
	( \+ Body = (user:true) -> 
		( VsOr == s ->
			Head =.. [PName,Id,Args,Pin,Lc]
			;
			Head =.. [PName,Id,Args,VsOr,Pin,Lc]
		),
		Clause = (Head:-Body)
		;
		% write( user_error, body(Body) ), nl( user_error ),
		( VsOr == s ->
			Clause =.. [PName,Id,Args,A,A]
			;
			Clause =.. [PName,Id,Args,VsOr,A,A]
		)
	),
	portray_clause( Clause ),
	record_switch_1( T, VsOr, PName ).
	
de_module_body( (A,B), Vs, (DemA,DemB) ) :-
	!,
	de_module_body( A, Vs, DemA ),
	de_module_body( B, Vs, DemB ).
de_module_body( (A->B;C), Vs, Dem ) :-
	!,
	de_module_body( A, Vs, DemA ),
	de_module_body( B, Vs, DemB ),
	de_module_body( C, Vs, DemC ),
	Dem = (DemA->DemB;DemC).
de_module_body( (A;B), Vs, (DemA;DemB) ) :-
	!,
	de_module_body( A, Vs, DemA ),
	de_module_body( B, Vs, DemB ).
% de_module_body( (Expr::InA), A ) :- !,
	% ( is_distributiona( InA ) -> 
		% InA =.. [Pred|Args],
		% A =.. [Pred,Expr|Args]
		% ;
		% write( 'inappropriate use of : within body goal' ), nl, abort
	% ).
	% can also can de_module_body().
de_module_body( (Mod::InA), _Vs, A ) :- !,
	% with new syntax, ::, Mod should never be an atom.
	( atom(Mod) -> 
		% ( is_distributional( InA ) -> 
			A = InA
			% PrvA =.. [Pred,_|Args],
			% A = Mod:PrvA
			% ;
			% A = Mod:InA
			% % write( 'inappropriate use of : within body goal' ), nl, abort
		% )
		;
		( is_distributional( InA ) -> 
			InA =.. [Pred|Args],
			en_list( Mod, ModList ),
			A =.. [Pred,ModList|Args]
			;
			write( user_error, ':: used for non distributional goal' ),
			nl( user_error ), write( user_error, goal(Mod:InA) ), nl( user_error ), abort
		)
	).
de_module_body( InA, _Vs, A ) :-
	( is_distributional( InA ) -> 
		InA =.. [Pred|Args],
		A =.. [Pred,_|Args]
		;
		A = InA
	).

meta_ad_strip( PrpA, A ) :-
	( PrpA = ad:A -> 
		true
		; 
		A = PrpA
	).

de_module_vars_list( _Mod:MVLs, VLs ) :-
	!, de_module_vars_list1( MVLs,VLs ).
de_module_vars_list( MVLs, VLs ) :-
	is_list( MVLs ),
	!, de_module_vars_list1( MVLs, VLs ).
de_module_vars_list( MVLs, _VLs ) :-
	write( user_error, dont_know_how_to_de_modue_vars_list(MVLs) ), nl.

de_module_vars_list1( [], [] ).
de_module_vars_list1( [H|T], [F|R] ) :-
	( var(H) -> F = H
		; ( H = _M:F -> true 
			; F = H ) ),
	de_module_vars_list1( T, R ).


kv_tri_decompose( [], [], [], [] ).
kv_tri_decompose( [H1-H2-H3|T], [H1|T1], [H2|T2], [H3|T3] ) :-
	kv_tri_decompose( T, T1, T2, T3 ).

load_method_to_load_pname( cm, compile ).
load_method_to_load_pname( cn, consult ).
load_method_to_load_pname( compile, compile ).
load_method_to_load_pname( consult, consult ).
load_method_to_load_pname( Unk, _ ) :-
	Unk \== cm, Unk \== cn,
	Unk \== compile, Unk \== consult,
	werr( [['Unknown load identifier ',Unk,'.'],
	       ['Use either cm or cn.']] ),
	abort.


default_msd( Msd ) :-
	( bims_bb_get(msd,Msd) -> 
		( (Msd==rm;Msd==fm) ->
			true 
			;
			werr( [['Unrecognised model space definiton token \'',Msd,'\' in ',bims_bb_get(msd,Msd)]] ),
			abort
		)
		;
		Msd = rm,
		werr( [['Using default model space definition token (rm)']] )
	).

/* 
     Currently the only built is s_random/1.
     Cute use of the path.... Id+Rnd, allows to carry forward
     the Rnd from the sampling pass to lead-in phase of 
     probabilisticly backtracking.
     */

built_ins_to_file( Tmp ) :-
     bims_bb_get( cc, CC ),
     Srand = (
          s_random( Id+Rnd, [Id+Rnd|T]/T, Rnd ) :- 
               ( var(Id) ->
                    random( Rnd )
                    ;
                    true
               ),
     Id = CC
     ),
     next_cc( CC ),
     open( Tmp, append, Out ),
     nl( Out ),
     % we can flag this according to whether any s_random/1 calls appear. Here
     portray_clause( Out, Srand ), nl( Out ),
     close( Out ).
