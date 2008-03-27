:- module(config, [target/1, 
					get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3, 
					set_dlog_options/1, set_dlog_options/2]).

% target(sicstus).
target(swi).



%default_option(?Name, ?Value): compile time default preferences
%%%%%%%%%%%% Translator Options %%%%%%%%%%%%
default_option(statistics, no). %[yes, no] 
default_option(orphan, priority). %[normal, priority]
default_option(decompose, yes). %[yes, no]
default_option(allinone, no). %[yes, no]
default_option(indexing, yes). %[yes, no]
default_option(projection, yes). %[yes, no]
default_option(preprocessing, yes). %[yes, no]
default_option(ground_optim, yes). %[yes, no]
default_option(generate_abox, no). %[yes, no]
default_option(filter_duplicates, no). %[yes, no]


%%%%%%%%%%%% DIG Server Options %%%%%%%%%%%%
default_option(dig_server_port, 8080). %TODO HTTP server port? (közös server?)
default_option(dig_server_path, '/'). %DIG server elérése
default_option(dig_server_service_limit, 60). %a kiszolgálásra mennyit várjon

:- dynamic  current_option/2.

%get_dlog_option(+Name, ?Value): get non KB-specific options
get_dlog_option(Name, Value) :-
	current_option(Name, Value) -> true
	; default_option(Name, Value).

%get_dlog_option(+Name, +URI, ?Value): get KB-specific options
get_dlog_option(Name, URI, Value) :-
	atom_concat('option_', URI, OptURI),
	(
		current_predicate(OptURI/2),
		call(OptURI, Name, Value) -> true
	;
		current_option(Name, Value) -> true
	; 
		default_option(Name, Value)
	).

%set_dlog_option(+Name, +Value): set non KB-specific options
set_dlog_option(Name, Value) :-
	retractall(current_option(Name, _)),
	assert(current_option(Name, Value)).


%set_dlog_option(+Name, +URI, +Value): set non KB-specific options
set_dlog_option(Name, URI,  Value) :-
	atom_concat('option_', URI, OptURI),
	Pred1 =.. [OptURI, Name, _],
	retractall(Pred1),
	Pred2 =.. [OptURI, Name, Value],
	assert(Pred2).

%set_dlog_option(+[Name(Value)]): set non KB-specific options
set_dlog_options([]).
set_dlog_options([Opt | Opts]) :-
	Opt =.. [Name, Value],
	set_dlog_option(Name, Value),
	set_dlog_options(Opts).

%set_dlog_option(+[Name(Value)], +URI): set KB-specific options
set_dlog_options([], _URI).
set_dlog_options([Opt | Opts], URI) :-
	Opt =.. [Name, Value],
	set_dlog_option(Name, URI, Value),
	set_dlog_options(Opts, URI).
