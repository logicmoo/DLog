:- module(dlogger, [
				log/4,
				error/3,
				warning/3,
				info/3,
				detail/3]).


:- use_module(config, [get_dlog_option/2, get_dlog_option/3]).


%log(Severity, Module, Context, Message):
%	Severity: error|warning|info|detail
%	Module: source module (atom)
%	Context: message context (structure)
%	Message: log message (atom)
log(error, Module, Context, Message) :-
	get_dlog_option(logging_detail, Module, silent) -> true
	;
	get_dlog_option(logfile, L),
	format(L, 'Error: ~a Context: ~a:~q.~n', [Message, Module, Context]).
log(warning, Module, Context, Message) :-
	get_dlog_option(logging_detail, Module, D),
	(	D == silent
	->	true
	;	D == error
	->	true
	;	get_dlog_option(logfile, L),
		format(L, 'Warning: ~a Context: ~a:~q.~n', [Message, Module, Context])
	).
log(info, Module, Context, Message) :-
	get_dlog_option(logging_detail, Module, D),
	(	D == silent
	->	true
	;	D == error
	->	true
	;	D == warning
	->	true
	;	get_dlog_option(logfile, L),
		format(L, 'Info: ~a Context: ~a:~q.~n', [Message, Module, Context])
	).
log(detail, Module, Context, Message) :-
	get_dlog_option(logging_detail, Module, D),
	(	D == detail
	->	get_dlog_option(logfile, L),
		format(L, 'Detail: ~a Context: ~a:~q.~n', [Message, Module, Context])
	;	true
	).


error(Module, Context, Message) :-
	log(error, Module, Context, Message).

warning(Module, Context, Message) :-
	log(warning, Module, Context, Message).

info(Module, Context, Message) :-
	log(info, Module, Context, Message).

detail(Module, Context, Message) :-
	log(detail, Module, Context, Message).




