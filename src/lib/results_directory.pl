
:- requires( os_slashify/2 ).

results_directory( SubDir, Full ) :-
     results_directory( '/tmp/', SubDir, Full ).

results_directory( Alternative, SubDir, Full ) :-
     ( environ('mcmcms_results_directory',Repo) ->
          os_slashify( Repo, RepoSl ),
          atom_concat( RepoSl, SubDir, Full )
          ;
          os_slashify( Alternative, AlternativeSl ),
          atom_concat( AlternativeSl, SubDir, Full )
     ).
