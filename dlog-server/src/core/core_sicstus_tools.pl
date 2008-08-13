:- module(core_sicstus_tools, [
				abs_file_name/3, 
				mutex_create/1, with_mutex/2, mutex_lock/1, mutex_unlock/1,
				call/2, call/3,
				setup_and_call_cleanup/3,
				format_to_atom/3]).

:- meta_predicate
	with_mutex(+, :),
	setup_and_call_cleanup(:, :, :).

:- use_module(library(charsio), [format_to_chars/3]).

abs_file_name(R, O, A) :- absolute_file_name(R, A, O).

mutex_create(_Mutex).
with_mutex(_Mutex, Goal) :- call(Goal).
mutex_lock(_Mutex).
mutex_unlock(_).

%not exactly like SWI, but works...
call(M:N, A) :-
	Goal =.. [N, A],
	call(M:Goal).

call(M:N, A1, A2) :-
	Goal =.. [N, A1, A2],
	call(M:Goal).

setup_and_call_cleanup(Setup, Goal, Cleanup) :-
	call(Setup),
	call_cleanup(Goal, Cleanup).

format_to_atom(Atom, Format, Params) :- 
	format_to_chars(Format, Params, Chars),
	atom_codes(Atom, Chars).
