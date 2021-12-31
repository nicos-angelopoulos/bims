:- pl( swi(_), 
		% ensure_loaded(swi_directory_files),
        % this is a built-in now (21.12.30)
        true,
		ensure_loaded(library(system))
	).
