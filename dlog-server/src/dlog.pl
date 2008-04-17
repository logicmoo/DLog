:- module(dlog, [tell_dig/1, execute_dig_file/2, 
					start_dig_server/0, stop_dig_server/0,
					get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					create_binary/0]).

:- use_module(kb_manager, [new_kb/1, release_kb/1, add_axioms/2]).
:- use_module('interfaces/dig_reader', [read_dig/2]).
:- use_module('interfaces/dig_iface', [execute_dig_file/2, start_dig_server/0, stop_dig_server/0]).
:- use_module(config, [get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					set_dlog_options/1]).

%DIGF : file name or stream(Stream)
% tell_dig('../examples/iocaste/c10.dig').
tell_dig(DIGF) :-
	read_dig(DIGF, tells(_, Axioms)),
	new_kb(URI),
	call_cleanup(
		add_axioms(URI, Axioms),
		release_kb(URI)
	).




create_binary :- qsave_program('../bin/dlog', [stand_alone(true), 
												goal(start_dlog), 
												toplevel(prolog)]).

:- multifile user:file_search_path/2.
user:file_search_path(foreign, 'lib').

start_dlog :- 
	print('Starting DLog...\n'),
	get_dlog_option(base_path, P),
	get_dlog_option(config_file, F),
	absolute_file_name(F, [relative_to(P)], File),
	open(File, read, S), 
	call_cleanup(read(S, T), close(S)),
	set_dlog_options(T),
	print('Config file loaded.\n'),
	start_dig_server,
	print('Server started.\n').




