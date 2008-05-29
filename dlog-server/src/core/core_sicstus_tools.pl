:- module(core_sicstus_tools, [
				abs_file_name/3, 
				mutex_create/1, with_mutex/2, mutex_lock/1, mutex_unlock/1]).

:- meta_predicate
	with_mutex(+, :).

abs_file_name(R, O, A) :- absolute_file_name(R, A, O).

mutex_create(_Mutex).
with_mutex(_Mutex, Goal) :- call(Goal).
mutex_lock(_Mutex).
mutex_unlock(_).

