:- module(dlog, [tell_dig/1]).

:- use_module(kb_manager, [new_kb/1, release_kb/1, add_axioms/2]).
:- use_module('interfaces/dig_reader', [read_dig/2]).

%DIGF : file name or stream(Stream)
tell_dig(DIGF) :-
	read_dig(DIGF, tells(_, Axioms)),
	new_kb(URI),
	call_cleanup(
		add_axioms(URI, Axioms),
		release_kb(URI)
	).
