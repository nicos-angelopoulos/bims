
:- ensure_loaded( parameter_metrics ).

:- requires( csv_df/2 ).
:- requires( en_list/2 ).

:- use_module( library(real) ).
:- 
	prolog_load_context( directory, Dir ),
	<- library("plyr"),
	<- library("ggplot2"),
	directory_file_path( Dir, 'summarySE.R', Path ),
	<- source( +Path ).

parameter_metrics_plots_defaults( [] ).

parameter_metric_plot_defaults( Args, Defs ) :-
	Args = [Mname|Rest],
	memberchk( parameter_names(Pnames), Rest ),
	( Pnames = [Pname1,Pname2] -> true; Pname1 = Pnames, Pname2='' ),
	( memberchk(ylab_prefix(Ypfx),Args) ->
		atom_concat( Ypfx, Mname, Ylab )
		;
		Ylab = Mname
	),
	Defs = [ylab(Ylab),xlab(Pname1),llab(Pname2),
	        exts(pdf)
		  ].


parameter_metrics_plots( ChPred, RedPred, ArgS ) :-
    en_list( ArgS, Args ),
    parameter_metrics_plots_defaults( Defs ),
    append( Args, Defs, Opts ),
	% options_append( parameter_metrics_plots, Args, Opts ),
	parameter_metrics( ChPred, RedPred, PMprs, Opts ),
	memberchk( metric_names(Mnames), Opts ),
	memberchk( parameter_names(Pnames), Opts ),
	maplist( parameter_metric_plot(Pnames,PMprs,Mnames,Opts), Mnames ).

%% parameter_metric_plot( Pnames, PMprs, Mnames, Opts, Mname ).
%
% Opts 
% * xlab(Xlab=f(Pnames))  x label, def: first name of Pnames
% * ylab(Ylab=Mname)      y label, the metric name
% * ylab_prefix(Ylab=Mname) prefixed to Mname to produce Ylab, if given
%
parameter_metric_plot( Pnames, PMprs, Mnames, ArgS, Mname ) :-
	% options_append( parameter_metric_plot, [Mname|Args], Opts ),
    en_list( ArgS, Args ),
    parameter_metric_plot_defaults( Args, Defs ),
    append( Args, Defs, Opts ),
	debug( _, 'Ploting metric: ~w', Mname ),
	once( nth1(N,Mnames,Mname) ),
	findall( Row, ( member(Pvals-Mvals,PMprs),
				 member(MvalsExp,Mvals),
				 nth1(N,MvalsExp,Mval),
	                Row=..[row,Mval|Pvals]
			    ),
			        Rows ),
	Hdr =.. [row,Mname|Pnames],
	csv_df( [Hdr|Rows], pmp_df ),
	<- pmp_df,
	maplist( atom_string, Pnames, Pstrings ),
	% pmp_dfs <- summarySE( pmp_df, measurevar=+Mname, groupvars=c("alpha","beta") ),
	Pnames = [First,Second],  % fixme: only works for 2
	% pmp_df$beta <- as.factor(pmp_df$beta),
	pmp_df$Second <- as.factor(pmp_df$Second),
	pmp_dfs <- summarySE( pmp_df, measurevar=+Mname, groupvars=Pstrings),
	% pd <- position_dodge(0.1),
	debug( real ),
	memberchk( xlab(Xlab), Opts ),
    memberchk( ylab(Ylab), Opts ),
    memberchk( llab(Llab), Opts ),
    
	<- print( 
	        ggplot(pmp_dfs, aes(x=First, y=Mname, colour=Second))
	         + geom_errorbar(aes(ymin=Mname-se, ymax=Mname+se), width=0.01 )
	         % ,position=pd
              + geom_line(aes(group=Second))
                % geom_line(position=pd) +
                % geom_point(position=pd),
              + geom_point()
		    + scale_color_discrete(name=+Llab)
		    + xlab(+Xlab) + ylab(+Ylab)
		     % + theme( legend.title = element_text(name="whatevs") )
		    % + opts(panel.background = theme_rect(fill="white", colour="black"))
		    +  theme_bw()
    ),
	<- print( pmp_dfs ),

	memberchk( exts(ExtS), Opts ),
	to_list( ExtS, Exts ),
	maplist( gg_save_stem_ext(Mname), Exts ).

gg_save_stem_ext( Mname, Ext ) :-
	file_name_extension( Mname, Ext, OutF ),
	<- ggsave(file=+OutF).
