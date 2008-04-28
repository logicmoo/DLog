:- module(dlog, [tell_dig/1, execute_dig_file/2, 
					start_dig_server/0, stop_dig_server/0,
					get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					create_binary/0, load_config_file/0]).

:- use_module(kb_manager, [new_kb/1, release_kb/1, add_axioms/2]).
:- use_module('interfaces/dig_reader', [read_dig/2]).
:- use_module('interfaces/dig_iface', [execute_dig_file/2, start_dig_server/0, stop_dig_server/0]).
:- use_module(config, [get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					set_dlog_options/1]).

%DIGF : file name or stream(Stream)
% tell_dig('../examples/iocaste/c10.dig').
tell_dig(DIGF) :-
	read_dig(DIGF, _NS-tells(_, Axioms)),
	new_kb(URI),
	call_cleanup(
		add_axioms(URI, Axioms),
		release_kb(URI)
	).




create_binary :- qsave_program('../bin/dlog', [stand_alone(true), 
												goal(start_dlog), 
												toplevel(halt(1))]).

:- multifile user:resource/3.
user:resource(dlog_hash, module, 'hash/hash.pl').
user:resource(hash_swi, module, 'hash/hash_swi.pl').


:- multifile user:file_search_path/2.
user:file_search_path(foreign, 'lib').

start_dlog :- 
	print('Starting DLog...\n'),
	load_config_file,
	print('Config file loaded.\n'),
	start_dig_server,
	print('Server started.\n'),
	console.

load_config_file :-
	get_dlog_option(base_path, P),
	get_dlog_option(config_file, F),
	absolute_file_name(F, [relative_to(P)], File),
	open(File, read, S), 
	call_cleanup(read(S, T), close(S)),
	set_dlog_options(T).

console :-
	help,
	repeat, 
	(catch(
		(
			read(X),
			nonvar(X),
			parse_console_command(X)
		), 
		error(syntax_error(_), _), 
		fail
	) 
	->
		%stop_dig_server,
		print('Server stopped.\n'),
		halt
	; 	help, 
		fail 
	).

help :-
	print('Type "quit." to quit.\n').

parse_console_command(quit).
parse_console_command(end_of_file).

%TODO: runtime entry
%TODO: sebesség, lib(list)

%TODO: összetett fogalmak ABoxban, kérdésben, összes szerep, fogalom, egyed

%cassertion rassertion

