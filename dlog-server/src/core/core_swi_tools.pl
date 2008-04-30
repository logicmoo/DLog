:- module(core_swi_tools, [datime/1]).

datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
	get_time(TimeStamp),
	stamp_date_time(TimeStamp, date(Year,Month,Day,Hour,Min,Sec1,_Off,_TZ,_DST), local),
	Sec is floor(Sec1).
