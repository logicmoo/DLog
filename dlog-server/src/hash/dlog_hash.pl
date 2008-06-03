%TODO: hash for anc list
:- module(dlog_hash, [init_state/1,
		     new_state/3,new_anc/3,new_loop/3,
		     check_anc/2,check_loop/2]).

sicstus_init :-
	%ensure_loaded(hash_sicstus),
	load_foreign_resource(hash_sicstus),
	use_module(library(lists), [member/2]),
	use_module(library(terms), [term_hash/4]).

swi_init :- 
	initialization(shlib:load_foreign_library(foreign(hash_swi), install)),
	(current_predicate(config:target/1)
	%open_resource(dlog_hash, module, H) 
	-> 
		%close(H),
		%library(lists) already loaded, may not be found on runtime version
		import(lists:member/2)
	; 
		%all-in-one file loaded, library(lists) should be accessable
		use_module(library(lists), [member/2])
	).


:- current_predicate(config:target/1)
	->
		(config:target(sicstus) -> sicstus_init ; true),
		(config:target(swi) -> swi_init ; true)
	;
		(current_prolog_flag(dialect, swi) -> swi_init 
		; %current_prolog_flag(language, sicstus) %sicstus/iso
		  %current_prolog_flag(version, 'SICStus...') %sicstus/iso
			sicstus_init
		)
	.

init_state(LoopHash-AncList) :-
	init_hash(LoopHash),
	AncList = [].

new_state(Goal, LH0-AL0, LH-AL) :-
	new_loop(Goal, LH0-_, LH-_), %TODO: only pass LH?
	AL = [Goal|AL0].

new_anc(Goal, LH-AL0, LH-AL) :-
	AL = [Goal|AL0].

check_anc(Goal, _-AL) :-
	member(Goal, AL).
