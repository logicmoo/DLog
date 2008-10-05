:- module(config, [target/1, 
					get_dlog_option/2, get_dlog_option/3, 
					get_kb_specific_options/2,
					set_dlog_option/2, set_dlog_option/3, 
					set_dlog_options/1, set_dlog_options/2,
					remove_dlog_options/1,
					default_kb/1, kb_uri/2,
					abox_module_name/2, tbox_module_name/2,
					abox_file_name/2, tbox_file_name/2,
					load_config_file/0, load_config_file/1]).

target(T) :- current_prolog_flag(dialect, swi) -> T = swi ; T = sicstus.
% target(sicstus).
% target(swi).

:- target(sicstus) -> 
	use_module(core_sicstus_tools, [abs_file_name/3, format_to_atom/3, call/3])
	; true.
:- target(swi) -> 
	use_module(core_swi_tools, [abs_file_name/3, format_to_atom/3])
	; true.

:- use_module(dlogger, [error/3, info/3]).


%default_option(?Name, ?Value): compile time default preferences
%%%%%%%%%%%% System Options %%%%%%%%%%%%
default_option(name, 'DLog'). %program name
default_option(version, '0.2 alpha'). %program version
default_option(description, D) :-
	get_dlog_option(name, N), 
	get_dlog_option(version, V), 
	get_dlog_option(server_host, H), 
	get_dlog_option(server_port, P), 
	format_to_atom(D, '~a ~a running on ~a:~d.', [N, V, H, P]).
default_option(base_path, './').
default_option(output_path, '../output/'). %Output directory (relative to base_path)
default_option(config_file, 'dlog.conf'). %config file name (relative to base_path)
default_option(binary_name, '../bin/dlog'). %binary directory (relative to base_path)
default_option(lib_path, 'hash'). %library path (relative to base_path)

default_option(logging_detail, warning). %[detail, info, warning, error, silent]
default_option(logfile, user_error). %open, writeable stream 

default_option(dlog_test_timeout, 60000). %time limit for each test phase (milliseconds)

%%%%%%%%%%%% Translator Options %%%%%%%%%%%%
default_option(statistics, no). %[yes, no] 
default_option(orphan, priority). %[normal, priority]
default_option(decompose, yes). %[yes, no]
default_option(indexing, yes). %[yes, no]
default_option(projection, yes). %[yes, no]
default_option(preprocessing, yes). %[yes, no]
default_option(ground_optim, yes). %[yes, no]
default_option(filter_duplicates, no). %[yes, no]

default_option(calculus, pure). %[dl, fol, old, pure]
default_option(logic_unfold, no). %[yes, no]

default_option(unfold, no). %[no, ?]

%assert: assert to module
%tempfile: create temporary memory/disk file
%allinonefile: create standalone prolog file
default_option(abox_target, assert). %[assert, tempfile, allinonefile]
default_option(tbox_target, tempfile). %[tempfile, allinonefile]

%%%%%%%%%%%% Server Options %%%%%%%%%%%%
default_option(server_port, 8080).
default_option(server_host, localhost).
default_option(dig_server_path, '/'). %path to DIG server
default_option(dig_server_service_limit, 60). %time limit for servicing a request (seconds)
default_option(dig_reader_fault_tolerance, no). %[no, drop, yes] a nem támogatott fogalmakra dobjon-e hibát

:- dynamic current_option/2.
:- volatile current_option/2. %don't save runtime options

%get_dlog_option(+Name, ?Value): get non KB-specific options
get_dlog_option(Name, Value) :-
	current_option(Name, Val) -> Val = Value
	; default_option(Name, Value).

%get_dlog_option(+Name, +URI, ?Value): get KB-specific options
get_dlog_option(Name, URI, Value) :-
	atom(URI),
	atom_concat('option_', URI, OptURI),
	(
		current_predicate(OptURI/2),
		call(OptURI, Name, Val) -> Val = Value
	;
		current_option(Name, Val) -> Val = Value
	; 
		default_option(Name, Value)
	).

%Options is a list of compile options overrided for KB URI.
get_kb_specific_options(URI, Options) :-
	atom(URI),
	atom_concat('option_', URI, OptURI),
	(	current_predicate(OptURI/2)
	->	findall(Option, 
			(	call(OptURI, Name, Value), 
				functor(Option, Name, 1), 
				arg(1, Option, Value)
			), Options)
	; Options = []
	).

%set_dlog_option(+Name, +Value): set non KB-specific options
set_dlog_option(Name, Value) :-
	atom(Name),
	nonvar(Value),
	%ground(Value),
	retractall(current_option(Name, _)),
	assert(current_option(Name, Value)).

%set_dlog_option(+Name, +URI, +Value): set KB-specific options
set_dlog_option(Name, URI,  Value) :-
	atom(Name),
	atom(URI),
	nonvar(Value),
	%ground(Value),
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

%remove_dlog_options(+URI): remove KB-specific options
remove_dlog_options(URI) :-
	atom(URI),
	atom_concat('option_', URI, OptURI),
	abolish(OptURI/2).

%uri_prefix('dlog://'). %{http://|dlog://}[<server>:<port>/]
uri_prefix(U) :-
	get_dlog_option(server_host, H), 
	get_dlog_option(server_port, P), 
	format_to_atom(U, 'http://~a:~d/', [H, P]).
kb_uri(ID, URI) :- 
	uri_prefix(Prefix),
	atom_concat(Prefix, ID, URI).
default_kb(URI) :- %dig 1.0 KB
	kb_uri('0', URI).


%abox_module_name(+URI, -Module): Module name for ABox of URI
abox_module_name(URI, Module) :-
	atom_concat(URI, '_abox', Module).

tbox_module_name(URI, Module) :-
	atom_concat(URI, '_tbox', Module).

abox_file_name(URI, File) :- %TODO: custom file name/KB?
	kb_uri(ID, URI),
	get_dlog_option(base_path, BP),
	get_dlog_option(output_path, OP),
	atom_concat(BP, OP, Path),
	atom_concat('abox_', ID, F1),
	atom_concat(F1, '.pl', F2),
	abs_file_name(F2, [relative_to(Path)], File).

tbox_file_name(URI, File) :-
	kb_uri(ID, URI),
	get_dlog_option(base_path, BP),
	get_dlog_option(output_path, OP),
	atom_concat(BP, OP, Path),
	atom_concat('tbox_', ID, F1),
	atom_concat(F1, '.pl', F2),
	abs_file_name(F2, [relative_to(Path)], File).

%load the config file
load_config_file :-
	get_dlog_option(base_path, P),
	get_dlog_option(config_file, F),
	abs_file_name(F, [relative_to(P)], File),
	load_config_file(File).
load_config_file(File) :-
	%access_file(File, read)
	abs_file_name(File, [access(read), file_errors(fail)], _)
	-> 	catch(
			(	open(File, read, S),
				call_cleanup(read(S, T), close(S)),
				set_dlog_options(T),
				info(config, load_config_file(File), 'Config file loaded.')
			),
			E,
			error(config, (load_config_file(File) --> E), 'Error loading config file.')
		)
	;	error(config, load_config_file(File), 'Could not open config file.').

