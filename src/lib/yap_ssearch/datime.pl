:- ensure_loaded( file_to_list_of_codes ).
:- ensure_loaded( three_letter_months ).
:- ensure_loaded( library(lists) ).

datime( datime(Year,Month,Day,Hour,Min,Sec) ) :-
	system( 'date | cat  > tmp_date_file' ),
	file_to_list_of_codes( 'tmp_date_file', DateLineCs ),
	append( _DayWord, [0' |Dlc1], DateLineCs ),
	append( MonthWordCs, [0' |Dlc2], Dlc1 ),
	atom_codes( MonthWord, MonthWordCs ),
	three_letter_months( MonthWords ),
	nth( Month, MonthWords, MonthWord ),
	append( DayCs, [0' |Dlc3], Dlc2 ),
	number_codes( Day, DayCs ),
	append( HourCs, [0':|Dlc4], Dlc3 ),
	number_codes( Hour, HourCs ),
	append( MinCs, [0':|Dlc5], Dlc4 ),
	number_codes( Min, MinCs ),
	append( SecCs, [0' |Dlc6], Dlc5 ),
	number_codes( Sec, SecCs ),
	append( _TmQual, [0' |Dlc7], Dlc6 ),
	append( YearCs, "\n", Dlc7 ),
	number_codes( Year, YearCs ),
	system( 'rm tmp_date_file' ),
	!.
