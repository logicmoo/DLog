:- module(core_swi_tools, [datime/1, abs_file_name/3, format_to_atom/3]).

datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
	get_time(TimeStamp),
	stamp_date_time(TimeStamp, date(Year,Month,Day,Hour,Min,Sec1,_Off,_TZ,_DST), local),
	Sec is floor(Sec1).

abs_file_name(Rel, Options, Abs) :- absolute_file_name(Rel, Options, Abs).

format_to_atom(Atom, Format, Params) :- format(atom(Atom), Format, Params).
