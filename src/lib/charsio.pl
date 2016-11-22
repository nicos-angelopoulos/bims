% Minimalistic support for SWI.

write_to_chars( Term, Codes ) :-
     term_to_atom( Term, Atom ),
     atom_codes( Atom, Codes ).
