:- module(dlog_test_swi_tools, [time_limit/2, time_limit/3]).

:- use_module('../core/config', [get_dlog_option/2, get_dlog_option/3]).

:- module_transparent time_limit/2, time_limit/3, time_limit0/3.

time_limit(Goal, Result) :-
	get_dlog_option(dlog_test_timeout, TO),
	time_limit0(Goal, TO, Result).

time_limit(Goal, URI, Result) :-
	get_dlog_option(dlog_test_timeout, URI, TO),
	time_limit0(Goal, TO, Result).

time_limit0(Goal, TO, Result) :-
	(	integer(TO), 
		TO > 0
	->	TO1 is TO / 1000,
		catch(
			(	call_with_time_limit(TO1, Goal),
				Result = success
			),
			time_limit_exceeded,
			Result = time_out
		)
	;	call(Goal),
		Result = success
	).

