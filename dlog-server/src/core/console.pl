:- module(console, [console/0]).

:- use_module('config', [get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					load_config_file/1]).
:- use_module('../interfaces/dig_iface', [execute_dig_file/2]).
:- use_module('../dlog', [start_server/0, stop_server/0]).
:- use_module('../test/dlog_test', [execute_test_files/2]).


console :-
	print('Type "quit." to quit, "help." for help.\n'),
	repeat, 
	catch(
		(
			read(X),
			nonvar(X),
			(parse_console_command(X) -> true
			; print('Error in command.\n'))
		), 
		_E,
		fail
	),
	fail.

dlog_help :-	
	get_dlog_option(description, D),
	format('~a~n~n', D),
	% print('Commands can be separated with commas (,), and end with a dot (.).\n\n'),
	print('Available commands:\n'),
	print('quit: stop the server and quit.\n'),
	print('help: print this help.\n'),
	print('start_server: starts the server.\n'),
	print('stop_server: stops the server.\n'),
	print('load_config_file(F): load the config file F.\n'),
	print('show_option(K): show the current value of the option K.\n'),
	print('show_option(K, URI): show the current value of the option K for the knowledge base identified by the URI.\n'),
	print('set_option(K, V): set the current value of the option K to V.\n'),
	print('set_option(K, URI, V): set the current value of the option K for the knowledge base identified by the URI to V.\n'),
	print('execute_dig_file(F): execute commands from the DIG file F and print the result state.\n'),
	print('execute_dig_filev(F): execute commands from the DIG file F and print the detailed results.\n'),
	%print('execute_dig_file(F, R): execute commands from the DIG file F and return the results in R.\n'). %TODO?
	print('execute_test_files(F, Mode): execute tests specified in files F. Mode is the output mode.\n').
	% print('print(P): print the expression P.\n'),
	% print('nl: print a new line.\n'),
	% print('\nexample: execute_dig_file(\'iocaste_tells.dig\', _), execute_dig_file(\'iocaste_asks.dig\', R), print(R).\n').

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
	print(R),
	nl.
parse_console_command(execute_dig_file(F)) :- !,
	catch(
		execute_dig_file(F, R),
		E,
		(format('~w~n', E), fail)
	),
	R =.. [H|_],
	print(H),
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

% parse_console_command(print(R)) :- !,
	% print(R),
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
	print('Type "quit." to quit, "help." for help.\n').

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

