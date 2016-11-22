%%   atom_dir_term( +DirAtom, -DirTerm )
%%   atom_dir_term( -DirAtom, +DirTerm )
%%   atom_dir_term( +DirAtom, +DirTerm )
%
%         Convert between 
%     ==
%          atom_dir_term( '.', Term ).
%          atom_dir_term( './abc', Term ).
%     ==
%
atom_dir_term( Dir, DirTerm ) :-
     \+ var(Dir),
     !,
     file_directory_name( Dir, Parent ), 
     parent_dir_to_term( Parent, Dir, DirTerm ).
atom_dir_term( Dir, DirTerm ) :-
     \+ var(DirTerm),
     DirTerm = Term/Base,
     !,
     dir_term_atom( Term, Base, Dir ).
atom_dir_term( Dir, Term ) :-
     atomic( Term ),
     Dir = Term.


