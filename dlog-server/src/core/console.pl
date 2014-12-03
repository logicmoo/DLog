:- module(console, [console/0]).

:- use_module('config', [get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					load_config_file/1]).
:- use_module('../interfaces/dig_iface', [execute_dig_file/2]).
:- use_module('../dlog', [start_server/0, stop_server/0]).
:- use_module('../test/dlog_test', [execute_test_files/2]).

cprint(X):-format(X).

console :-
	cprint('Type "quit." to quit, "help." for help.\n'),
	repeat, 
	catch(
		(
			read(X),
			nonvar(X),
			(parse_console_command(X) -> true
			; cprint('Error in command.\n'))
		), 
		_E,
		fail
	),
	fail.

dlog_help :-	
	get_dlog_option(description, D),
	format('~a~n~n', D),
	% cprint('Commands can be separated with commas (,), and end with a dot (.).\n\n'),
	cprint('Available commands:\n'),
	cprint('quit: Stop the server and quit DLog.\n'),
	cprint('help: Print this help.\n'),
	cprint('start_server: Start the server.\n'),
	cprint('stop_server: Stop the server.\n'),
	cprint('load_config_file(F): Load the configuration file F.\n'),
	cprint('show_option(K): Show the current value of the option K.\n'),
	cprint('show_option(K, URI): Show the current value of the option K for the knowledge base identified by the URI.\n'),
	cprint('set_option(K, V): Set the value of the option K to V.\n'),
	cprint('set_option(K, URI, V): Set the value of the option K for the knowledge base identified by the URI to V.\n'),
	cprint('execute_dig_file(F): Execute a DIG command from the file F and cprint the result state.\n'),
	cprint('execute_dig_filev(F): Execute a DIG command from the file F and cprint the detailed results.\n'),
	%cprint('execute_dig_file(F, R): Execute a DIG command from the file F and return the results in R.\n'). %TODO?
	cprint('execute_test_files(F, Mode): Execute tests specified in files F. Mode is the output mode.\n').
	% cprint('cprint(P): cprint the expression P.\n'),
	% cprint('nl: cprint a new line.\n'),
	% cprint('\nexample: execute_dig_file(\'iocaste_tells.dig\', _), execute_dig_file(\'iocaste_asks.dig\', R), cprint(R).\n').

parse_console_command(load_config_file(F)) :- !,
	load_config_file(F). %TODO start server, stop server, get/set_option
parse_console_command(show_option(K)) :- !,
	atom(K),
	get_dlog_option(K, V),
	format('The value of \'~a\' is \'~w\'.\n', [K, V]).
parse_console_command(show_option(K, URI)) :- !,
	atom(K),
	get_dlog_option(K, URI, V),
	format('The value of \'~a\' for knowledge base \'~a\' is \'~w\'.\n', [K, URI, V]).
parse_console_command(set_option(K, V)) :- !,
	set_dlog_option(K, V),
	format('The value of \'~a\' is set to \'~w\'.\n', [K, V]).
parse_console_command(set_option(K, URI, V)) :- !,
	set_dlog_option(K, URI, V),
	format('The value of \'~a\' for knowledge base \'~a\' is set to \'~w\',\n', [K, URI, V]).

parse_console_command(start_server) :- !,
	start_server.
parse_console_command(stop_server) :- !,
	stop_server.

parse_console_command(execute_dig_filev(F)) :- !,
	catch(
		execute_dig_file(F, R),
		E,
		(format('~w~n', E), fail)
	),
	cprint(R),
	nl.
parse_console_command(execute_dig_file(F)) :- !,
	catch(
		execute_dig_file(F, R),
		E,
		(format('~w~n', E), fail)
	),
	R =.. [H|_],
	cprint(H),
	nl.
% parse_console_command(execute_dig_file(F, R)) :- !,
	% catch(
		% execute_dig_file(F, R),
		% E,
		% R=E
	% ).

parse_console_command(execute_test_files(F)) :- !,
	execute_test_files(F, text).

parse_console_command(execute_test_files(F, Mode)) :- !,
	(	test_output_mode(Mode) 
	->	execute_test_files(F, Mode)
	;	write('Invalid output mode.\n'),
		write('Valid modes are text for displaying results here or text(File) to write the results to a file.\n'),
		write('Mode can be omitted for text output.\n')
	).

parse_console_command(quit) :- !,
	halt.
parse_console_command(end_of_file) :- !,
	halt.

% parse_console_command(cprint(R)) :- !,
	% cprint(R),
	% nl.
% parse_console_command(nl) :- !,
	% nl.
% parse_console_command((A,B)) :-
	% parse_console_command(A), 
	% !, 
	% parse_console_command(B).
parse_console_command(help) :- !,
	dlog_help.
parse_console_command(prolog) :- !,
	prolog.
parse_console_command(A) :- 
	format('Unknown command "~w".~n', A),
	cprint('Type "quit." to quit, "help." for help.\n').

test_output_mode(text).
test_output_mode(text(Out)) :- 
	atom(Out).
% test_output_mode(html).
% test_output_mode(html(Out)) :- 
	% atom(Out).
% test_output_mode(xml).
% test_output_mode(xml(Out)) :- 
	% atom(Out).
% test_output_mode(latex).
% test_output_mode(latex(Out)) :- 
	% atom(Out).

