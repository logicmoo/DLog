:- module(unfold_main, [unfold_predicates/2]).

:- use_module('../core/config', [target/1]).

:- target(sicstus) -> ensure_loaded(unfold_sp)
	; true.

:- target(swi) -> ensure_loaded(unfold_swi)
	; true.



