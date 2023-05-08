
:- use_module(library(prolog_pack)).

:- use_module(library(lib)).

:- lib(os_lib).
:- lib(options).
:- lib(debug_call).

distro_defaults([debug(true),distro(true)]).

/** distro(+Opts).

This scripts tests a new Bims distro before it is shippped out.

It can also be used to test the installed version (=|Distro=false|=).
When =|Distro=true|= the script looks for latest .tgz in ~/web/sware/packs/bims
and works on that. Currently the script runs 2 defaults bim chains (making sure
that the distribution runs cleanly for multiple chains) and tests dlp_ sampling 
and probabilistic inference, again with loading multiple dlp files ensuring
this is working properly- seen as it is one of the ore delicate operations

The script depends on a number of https://stoics.org.uk/~nicos/sware/packs
which are orchestrated by pack(lib). The user will be asked for each
missing pack if they want to be installed. If you rather not install
additional code, it is best you do not use this script.

Opts
  * debug(Dbg=false)
    informational, progress messages
  * distro(Distro=true)
    set to false to test the installed version

Examples
==
?- distro([]).
==

From comand line:
==
% upsh distro

%the above gets stuck at 
     % Cleaning module: ad
==

Full run for bims-3.0
==
?- distro([]).
% Ords: [bims-3.0.tgz,bims-2.0.tgz]
% Sel: 'bims-3.0.tgz'
% Removing '/home/nicos/.local/share/swi-prolog/pack/bims' and contents
% Installing: '/home/nicos/web/sware/packs/bims/bims-3.0.tgz'
% Testing installed bims version: 3.0
% Run: 1st def chain
% Results directory: /tmp/swipl_bims_distro_29101_1/res-23.05.08
% Cleaning module: bims_lkl
% Looking for likelihood in file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts.pl
% Loading likelihood from file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts.pl
% Cleaning module: data
% Bims data dirs: [/home/nicos/.local/share/swi-prolog/pack/bims/data]
% Testing existance of data file: data/carts.pl
% Testing existance of data file: /home/nicos/.local/share/swi-prolog/pack/bims/data/carts.pl
% Loading data file: /home/nicos/.local/share/swi-prolog/pack/bims/data/carts.pl
% Looking data preparation file in: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts_data.pl
% Loading data preparation file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts_data.pl
% Data categories: [2,4]
% Looking for prior in file: dlps/cart.dlp
% Looking for prior in file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/dlps/cart.dlp
% Will be using prior in file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/dlps/cart.dlp
% Run: 1
% Seed: 1 (-> rand(6548,19229,23110))
% Cleaning module: tmp
% Cleaning module: slp
% Cleaning module: ad
..........
% Run: 2
% Seed: 2 (-> rand(12077,7255,10019))
% Cleaning module: tmp
% Cleaning module: slp
% Cleaning module: ad
..........
% Run: 3
% Seed: 3 (-> rand(25652,20777,8216))
% Cleaning module: tmp
% Cleaning module: slp
% Cleaning module: ad
..........
% Finished all runs
% Done: bims
% Run: 2nd def chain
% Results directory: /tmp/swipl_bims_distro_29101_1/res-23.05.08.20.40
% Cleaning module: bims_lkl
% Looking for likelihood in file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts.pl
% Loading likelihood from file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts.pl
% Cleaning module: data
% Bims data dirs: [/home/nicos/.local/share/swi-prolog/pack/bims/data]
% Testing existance of data file: data/carts.pl
% Testing existance of data file: /home/nicos/.local/share/swi-prolog/pack/bims/data/carts.pl
% Loading data file: /home/nicos/.local/share/swi-prolog/pack/bims/data/carts.pl
% Looking data preparation file in: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts_data.pl
% Loading data preparation file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/lklhoods/carts/carts_data.pl
% Data categories: [2,4]
% Looking for prior in file: dlps/cart.dlp
% Looking for prior in file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/dlps/cart.dlp
% Will be using prior in file: /home/nicos/.local/share/swi-prolog/pack/bims/models/carts/dlps/cart.dlp
% Run: 1
% Seed: 1 (-> rand(6548,19229,23110))
% Cleaning module: tmp
% Cleaning module: slp
% Cleaning module: ad
..........
% Run: 2
% Seed: 2 (-> rand(12077,7255,10019))
% Cleaning module: tmp
% Cleaning module: slp
% Cleaning module: ad
..........
% Run: 3
% Seed: 3 (-> rand(25652,20777,8216))
% Cleaning module: tmp
% Cleaning module: slp
% Cleaning module: ad
..........
% Finished all runs
% Done: bims
% Run: 1st load: doubles
% Done: dlp_load(doubles)
% Run: sample prob, free var
% Done: dlp_sample(doubles(head),[3:1,1/0.5,1:0.5],0.25)
% Run: 2nd load: umember
% Done: dlp_load(umember)
% Run: umember/2 svg plot
% Loading installed R library: NMF
% Loading installed R library: ggplot2
% Loading installed R library: ggpubr
% Loading installed R library: gridExtra
% Done: lib(mlu),mlu_sample(dlp_sample(umember([a,b,c,d,e,f,g,h],_130)),1000,_130,[a-127,b-140,c-128,d-112,e-123,f-128,g-117,h-125]),mlu_frequency_plot([a-127,b-140,c-128,d-112,e-123,f-128,g-117,h-125],[interface(barplot),outputs(svg),las=2])
% Finished: distro
true.
==

@author nicos angelopoulos
@version  0.1 2023/05/08

*/

distro( Args ) :-
     Self = distro,
     options_append( Self, Args, Opts ),
     tmp_file( bims_distro, Tmp ),
     make_directory( Tmp ),
     working_directory( Old, Tmp ),
     debuc( Self, 'Working at: ~p', [Tmp] ),
     options( distro(Distro), Opts ),
     distro_opt( Distro, Self ),
     working_directory( _, Old ),
     debuc( Self, end, true ).

distro_installed( Self ) :-
     pack_property( bims, version(Vers) ),
     debuc( Self, 'Testing installed bims version: ~w', [Vers] ),
     use_module( library(bims) ),
     distro_testo( Self ).

distro_testo( Self ) :-
     distro_testo( Cond, Reas, Goal, Mess ),
     ( call(Cond) -> 
          debuc( Self, 'Run: ~w', [Mess] ),
          once( Goal ),
          debuc( Self, 'Done: ~w', [Goal] )
          ;
          debuc( Self, 'not Run: ~w', [Mess] ),
          debuc( Self, 'because: ~w', [Reas] )
     ),
     fail.
distro_testo( _ ).

distro_testo(true, '', bims, '1st def chain').
distro_testo(true, '', bims, '2nd def chain').
distro_testo(true, '', dlp_load(doubles), '1st load: doubles').
distro_testo(true, '', dlp_sample(doubles(_Coin),_Path,_Prb), 'sample prob, free var').
distro_testo(true, '', dlp_load(umember), '2nd load: umember').
distro_testo( pack_property(mlu,_), 'pack(mlu) not installed', Goal, 'umember/2 svg plot' ) :-
     Goal = ( lib(mlu),
              mlu_sample( dlp_sample(umember([a,b,c,d,e,f,g,h],X)), 1000, X, Freqs ),
              mlu_frequency_plot( Freqs, [interface(barplot),outputs(svg),las = 2])
            ).

distro_opt( true, Self ) :-
     !,
     Bir = '/home/nicos/web/sware/packs/bims',
     os_sel( os_files, ext(tgz), Sels, dir(Bir) ),
     sort( 0, @>=, Sels, Ords ),
     debuc( Self, 'Ords: ~w', [Ords] ),
     Ords = [Tgz|_],
     debuc( Self, 'Sel: ~p', [Tgz] ),
     pack_remove( bims ),
     directory_file_path( Bir, Tgz, AbsTgz ),
     debuc( Self, 'Installing: ~p', [AbsTgz] ),
     pack_install( AbsTgz ),
     distro_installed( Self ).
distro_opt( false, Self ) :-
     !,
     distro_installed( Self ).
distro_opt( Other, _Self ) :-
     throw( unknown_option_value(distro,Other) ).
