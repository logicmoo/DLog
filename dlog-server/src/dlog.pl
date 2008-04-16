:- module(dlog, [tell_dig/1, execute_dig_file/2, 
					start_dig_server/0, stop_dig_server/0,
					get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3]).

:- use_module(kb_manager, [new_kb/1, release_kb/1, add_axioms/2]).
:- use_module('interfaces/dig_reader', [read_dig/2]).
:- use_module('interfaces/dig_iface', [execute_dig_file/2, start_dig_server/0, stop_dig_server/0]).
:- use_module(config, [get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3]).

%DIGF : file name or stream(Stream)
% tell_dig('../examples/iocaste/c10.dig').
tell_dig(DIGF) :-
	read_dig(DIGF, tells(_, Axioms)),
	new_kb(URI),
	call_cleanup(
		add_axioms(URI, Axioms),
		release_kb(URI)
	).
