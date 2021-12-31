:- ensure_loaded( library(lists) ).		% memberchk/2.

opts_have_help( Opts ) :-
     opts_have_help( Opts, _Width ).

opts_have_help( Opts, Width ) :-
	( (memberchk(h,Opts);memberchk(help,Opts);Opts=h;Opts==help) ->
		( memberchk(help_screen_width(Width),Opts) ->
               true
               ;
               Width is 80
          )
		;
	     ( (memberchk(h(W),Opts);memberchk(help(W),Opts);
             Opts=h(W);Opts==help(W)) ->
               Width = W
               ;
		     fail
          )
	).
