:- module(dlog_hash, [
				init_state/1, init_state/2, %TODO: OK? 
				%init_hash/1, init_hash/2, %foreign
				put_to_hash/3, check_hash/2, %foreign
				put_to_list/3, check_list/2,
				put_to_both/3, %TODO: kell?
				
				%temporary compatibility predicates
				new_state/3, %put_to_both/3
				new_anc/3, %put_to_list/3
				new_loop/3, %put_to_hash/3
				check_anc/2, %check_list/2
				check_loop/2 %check_hash/2
				]).

sicstus_init :-
	load_foreign_resource(hash_sicstus),
	use_module(library(lists), [member/2]).

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



%init_hash(LoopHash) :- foreign.

%init_state(-(LoopHash-AncList))
init_state(LoopHash-AncList) :-
	init_hash(LoopHash),
	AncList = [].

%init_hash(HashSize, LoopHash) :- foreign.

%init_state(+HashSize, -(LoopHash-AncList))
init_state(HashSize, LoopHash-AncList) :-
	init_hash(HashSize, LoopHash),
	AncList = [].

%put_to_hash(Goal, State0, State) :- foreign.
%check_hash(Goal, State) :- foreign.

put_to_list(Goal, LH-AL, LH-[Goal|AL]).
check_list(Goal, _-AL) :-
	member(Goal, AL).

put_to_both(Goal, LH0-AL, LH-[Goal|AL]) :- %TODO: is this used at all?
	put_to_hash(Goal, LH0-_, LH-_).


% Compatibility predicates
new_state(Goal, State0, State) :- put_to_both(Goal, State0, State).
new_anc(Goal, State0, State) :- put_to_list(Goal, State0, State).
check_anc(Goal, State) :- check_list(Goal, State).
new_loop(Goal, State0, State) :- put_to_hash(Goal, State0, State).
check_loop(Goal, State) :- check_hash(Goal, State).
