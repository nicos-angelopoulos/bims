%% open_mode( Mode, File, -Stream ).
%
% Argument rehash of open/3 that allows for opening a number of files 
% with the same mode in a maplist call.
%==
% ?- maplist( open_mode(write), [a,b,c], Streams ),
%    maplist( write, Streams, [x,y,z] ),
%    maplist( close, Streams ).
% ?- shell( 'more a' ).
% x 
% true.
%==
% @author nicos angelopoulos
% @version  0.1 2014/9/2
open_mode( Mode, File, Stream ) :-
	open( File, Mode, Stream ).
