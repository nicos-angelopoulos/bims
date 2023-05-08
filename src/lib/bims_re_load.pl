
% 23.05.08: after deleting likelihoods (and likely other mem modules), we need to really load, 
%           rather than ensure_loaded/1, which the system thinks is already loaded.
bims_re_load( File ) :-
     get_time(Stamp), 
     load_files( File, [modifiled(Stamp)] ).
