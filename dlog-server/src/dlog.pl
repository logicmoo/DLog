:- module(dlog, [
		execute_dig_file/2, %reexported from module dig_iface.
		start_server/0, stop_server/0, %start/stop the server.
		get_dlog_option/2, get_dlog_option/3, %reexported from module config.
		set_dlog_option/2, set_dlog_option/3, %reexported from module config.
		load_config_file/0, load_config_file/1, %reexported from module config.
		execute_test_files/2, %reexported from module dlog_test.
		create_binary/0 %create the stand-alone binary version of DLog.
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      STARTUP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% IMPORTANT: first load the config module, then load the config file, 
%  finally load the rest!
:- use_module('core/config', [target/1,
					get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					load_config_file/0, load_config_file/1]).

%load config at startup
:- initialization load_config_file.

:- use_module('core/kb_manager', [new_kb/1, release_kb/1]).
:- use_module('core/console', [console/0]).
:- use_module('interfaces/dig_iface', [execute_dig_file/2]).
:- use_module('core/dlogger', [error/3, warning/3, info/3, detail/3]).
:- use_module('test/dlog_test', [execute_test_files/2]).

:- target(swi) -> 
	use_module('core/core_swi_tools', [abs_file_name/3]),
	use_module(library('http/thread_httpd'), [http_server/2, http_stop_server/2]),
	use_module(library('http/http_error.pl'), [])	%debug module -> 500 w/ stacktrace
	; true.

:- target(sicstus) -> 
	use_module('core/core_sicstus_tools', [abs_file_name/3])
	; true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile user:file_search_path/2. 
user:file_search_path(foreign, LP) :- 
	get_dlog_option(base_path, B),
	get_dlog_option(lib_path, L),
	abs_file_name(L, [relative_to(B)], LP).

%SWI: resource, Sicstus: use_module/3
:- multifile user:resource/3. 
user:resource(dlog_hash, module, 'hash/dlog_hash.pl').


%create the stand-alone binary version of DLog.
create_binary :- 
	get_dlog_option(base_path, B),
	get_dlog_option(binary_name, RN),
	abs_file_name(RN, [relative_to(B)], N),
	qsave_program(N, [
		stand_alone(true), 
		goal(start_dlog), 
		init_file(none),
		toplevel(halt(1))
		% toplevel(prolog) %debug
		% local %stack sizes
		% global
		% trail
		% argument
	]).


%startup goal
start_dlog :-
	info(dlog, start_dlog, 'Starting DLog server...'),
	start_server,
	console.

%start the server.
start_server :-
	get_dlog_option(server_port, Port),
	http_server(http_dispatch:http_dispatch, [port(Port), timeout(30)]),
		%TODO: timeout(+SecondsOrInfinite): a kérésre mennyit várjon, 
		%workers(+N): hány szál (2)
		%after(:Goal) -> válasz után feldolgozás/statisztika
		%... (stack méret, ssl)
	format(atom(M), 'Server started on port ~d.', Port),
	warning(dlog, start_server, M).

%stop the server.
stop_server :- 
	get_dlog_option(server_port, Port), %TODO: mi van, ha változtattak rajta?
	http_stop_server(Port, _Options), %TODO: exception
	format(atom(M), 'Server stopped on port ~d.~n', Port),
	warning(dlog, stop_server, M).
	


