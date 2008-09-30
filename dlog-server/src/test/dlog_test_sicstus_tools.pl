:- module(dlog_test_sicstus_tools, [time_limit/2, time_limit/3, 
									expand_file_name/2, message_to_string/2,
									setup_and_call_cleanup/3, call/2]).

:- use_module('../core/config', [get_dlog_option/2, get_dlog_option/3]).
:- use_module(library(timeout), [time_out/3]).
:- use_module(library(system), [directory_files/2]).
:- use_module(library(charsio), [with_output_to_chars/4]).
:- use_module(library(lists), [append/3, member/2]).

:- meta_predicate 
	time_limit(:, -), 
	time_limit(:, +, -), 
	setup_and_call_cleanup(:, :, :),
	call(:, +).

time_limit(Goal, Result) :-
	get_dlog_option(dlog_test_timeout, TO),
	time_limit0(Goal, TO, Result).

time_limit(Goal, URI, Result) :-
	get_dlog_option(dlog_test_timeout, URI, TO),
	time_limit0(Goal, TO, Result).

time_limit0(Goal, TO, Result) :-
	(	integer(TO), 
		TO > 0
	->	time_out(Goal, TO, Result)
	;	call(Goal),
		Result = success
	).

setup_and_call_cleanup(Setup, Goal, Cleanup) :-
	call(Setup),
	call_cleanup(Goal, Cleanup).

expand_file_name(Pattern, Files) :-
	directory_files(Pattern, L),
	findall(N, tst_file(Pattern, L, N), Files).

tst_file(Pattern, List, Name) :-
	member(F, List), 
	absolute_file_name(F, Name, [relative_to(Pattern)]), 
	atom_concat(_, '.tst', Name).


call(Module:Goal0, Param) :-
   Goal0 =.. [Name | Params0],
   append(Params0, [Param], Params1),
   Goal =.. [Name | Params1],
   call(Module:Goal).

message_to_string(Message, String) :-
	prolog_flag(user_error, Stream0),
	with_output_to_chars(
		call_cleanup(
			m_t_s(Message, Stream),
			set_prolog_flag(user_error, Stream0)
		),
		Stream, String, []).

m_t_s(Message, Stream) :-
	set_prolog_flag(user_error, Stream),
	print_message(error, Message).








