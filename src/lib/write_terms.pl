
% :- lib(in_sub_dirs).                    % atom_dir_term/2.
:- lib(atom_dir_term/2).
:- lib(file_name_extends/3).
:- lib(en_list/2).

%% write_terms( File, Terms ).
%
%  Write Terms on File.
%
write_terms( FnameIn, Terms ) :-
     ( atom_dir_term(FnameAtom,FnameIn) ->
		true
		;
		atomic( FnameIn ),
		FnameAtom = FnameIn
	),
     once( file_name_extends(FnameAtom,pl,write,Fname) ),
	write_terms( Fname, write, Terms, [quoted(true)] ).

%% write_terms( File, +TermS, +OptS ).
%
%  Write TermS on File. TermS and OptS will be listified.
%
%  Options is a term or list of terms from:
%    * comment(CommTerm)   Comment is written at top of file before Terms.
%    * mode(WriteMode)     Writing mode: *write* or _append_.
%
% ==
%   ?- write_terms( testo, [ab,c(e),d], [comment(naku)] ).
%   true.
% 
%   ?- shell( 'more testo' ).
%
%   % naku
%   ab.
%   c(e).
%   d.
%
%   ?- read_terms( testo, Terms ).
%  Terms = [ab, c(e), d].
%   
% ==
%  
% @author  nicos angelopoulos
% @version 3.0, 2014/01/28
% @see  read_terms/2
%
write_terms( Fname, Terms, Args ) :-
	en_list( Args, Opts ),
	( memberchk(mode(Mode),Opts) -> true; Mode = write ),
	write_terms( Fname, Mode, Terms, [quoted(true)|Opts] ).
write_terms( File, Mode, InTerms, Opts ) :-
        open( File, Mode, Stream ),
	   ( memberchk(comment(Comm),Opts) -> write( Stream, '% ' ),
	                                      write( Stream, Comm ), nl( Stream )
	   							   ;
								   true
	   ),
	   en_list( InTerms, Terms ),
        write_terms_stream( Stream, Terms, Opts ),
        close( Stream ).

write_terms_stream( _Stream, [], _Opts ) :- !.
write_terms_stream( Stream, [H|T], Opts ) :-
	( H == [] ->
		nl( Stream )
		;
		write_term( Stream, H, Opts ),
		write( Stream, '.' ),
		nl( Stream )
	),
	write_terms_stream( Stream, T, Opts ).
