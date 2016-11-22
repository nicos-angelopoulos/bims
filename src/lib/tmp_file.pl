% This is defined in Swi Prolog.
% Note that this does NOT automatically delete the Temp file on exit.
%

:- ensure_loaded( library(system) ).

tmp_file( Stem, Tmp ) :-
     atom_concat( ['/tmp/', Stem,'XXXXXX'], Template ),
     mktemp( Template, Tmp ).

