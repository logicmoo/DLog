:- module(kb_manager, [
					new_kb/1, release_kb/1, add_axioms/2, run_query/3, %addTAxioms/2, addAAxioms/2 ?
					default_kb/1, clear_kb/1,
					with_read_lock/2, with_write_lock/2
					]).

:- use_module(library(lists), [append/3]).

:- use_module('dl_translator/translator', [axioms_to_clauses/5]). %TODO
:- use_module('prolog_translator/abox_signature', [abox_signature/3]).
:- use_module('prolog_translator/abox_translator', [abox2prolog/2]). %TODO
:- use_module('prolog_translator/tbox_translator', [tbox2prolog/3]).
:- use_module('prolog_translator/tbox_annotator', [annotated_tbox/2]).
:- use_module(query, [query/4]).
:- use_module(config, [target/1, get_dlog_option/3, default_kb/1, kb_uri/2, remove_dlog_options/1,abox_module_name/2, tbox_module_name/2, abox_file_name/2, tbox_file_name/2]).
:- target(swi) -> use_module(library(memfile)) ; true.

:- dynamic current_kb/1,
			kb_count/1.


:- assert(kb_count(1)), mutex_create(kb_count), default_kb(Def), mutex_create(Def), assert(current_kb(Def)). %initialization?


exists_kb(URI) :-
	nonvar(URI),
	current_kb(URI) -> true
	; throw(no_such_kb).

new_kb(URI) :- 
	with_mutex(kb_count,
	(
		retract(kb_count(ID)),
		ID1 is ID+1, %{<sorszám>|<UUID>}?
		assert(kb_count(ID1))
	)),
	kb_uri(ID, URI),
	mutex_create(URI),
	assert(current_kb(URI)).

release_kb(URI) :-
	exists_kb(URI),
	with_write_lock(URI,
	(
		clear_kb(URI),
		remove_dlog_options(URI), %beállítások törlése
		retract(current_kb(URI))
	))
	%,mutex_destroy(URI) %TODO
	.

clear_kb(URI) :- 
	tbox_module_name(URI, TB),
	abox_module_name(URI, AB),
	with_write_lock(URI,
	(
		retractall(AB:_), 
		retractall(TB:_) %TODO file-ok törlése?
		%, remove_dlog_options(URI) %TODO: beállítások törlése?
	)).


add_axioms(URI, axioms(ImpliesCL, ImpliesRL, TransL, ABox)) :- %TODO: eltárolni, hozzáadni
	exists_kb(URI),
	axioms_to_clauses([ImpliesCL, ImpliesRL, TransL],
			  TBox_Clauses0, IBox, HBox, _), %TODO
	annotated_tbox(TBox_Clauses0, TBox_Clauses), % annotated version of TBox
	abox_signature(ABox, ABoxStr, Signature),
	get_dlog_option(abox_target, URI, ATarget),
	get_dlog_option(tbox_target, URI, TTarget),
	with_write_lock(URI, 
	(	
		add_abox(ATarget, URI, abox(ABoxStr)),
		add_tbox(TTarget, URI, tbox(TBox_Clauses, IBox, HBox), abox(Signature))
	)).

add_abox(tempfile, URI, ABox) :-
	new_memory_file(AMemFile),
	current_output(Out),
	call_cleanup(
		(
			open_memory_file(AMemFile, write, AStream),
			set_output(AStream),
			call_cleanup(
				abox2prolog(URI, ABox), %TODO 
				(set_output(Out), close(AStream))
			),
			open_memory_file(AMemFile, read, AStream2),
			abox_module_name(URI, AB),
			call_cleanup(
				load_files(AB, [stream(AStream2)]), %TODO
				close(AStream2)
			)
		),
		free_memory_file(AMemFile)
	).
add_abox(allinonefile, URI, ABox) :-
	abox_file_name(URI, AFile),
	open(AFile, write, AStream),
	current_output(Out),
	set_output(AStream),
	call_cleanup(
		abox2prolog(URI, ABox), %TODO 
		(set_output(Out), close(AStream))
	),
	load_files(AFile, []). %TODO
add_abox(assert, URI, ABox) :-
	abox2prolog(URI, ABox).


add_tbox(tempfile, URI, TBox, ABox) :-
	new_memory_file(TMemFile),
	current_output(Out),
	call_cleanup(
		(
			open_memory_file(TMemFile, write, TStream),
			set_output(TStream),
			call_cleanup(
				tbox2prolog(URI, TBox, ABox), %TODO
				(set_output(Out), close(TStream))
			),
			open_memory_file(TMemFile, read, TStream2),
			tbox_module_name(URI, TB),
			call_cleanup(
				load_files(TB, [stream(TStream2)]), %TODO
				close(TStream2)
			)
		),
		free_memory_file(TMemFile)
	).
add_tbox(allinonefile, URI, TBox, ABox) :-
	tbox_file_name(URI, TFile),
	open(TFile, write, TStream),
	current_output(Out),
	set_output(TStream),
	call_cleanup(
		tbox2prolog(URI, TBox, ABox), %TODO
		(set_output(Out), close(TStream))
	),
	load_files(TFile, []). %TODO
add_tbox(assert, URI, TBox, ABox) :-
	tbox2prolog(URI, TBox, ABox).


run_query(Query, URI, Answer) :- 
	exists_kb(URI),
	tbox_module_name(URI, TBox),
	abox_module_name(URI, ABox),
	with_read_lock(URI,
	(
		query(Query, TBox, ABox, Answer)
	)).


get_read_lock(URI) :-
	mutex_lock(URI),
	exists_kb(URI).
get_write_lock(URI) :-
	mutex_lock(URI),
	exists_kb(URI).
release_read_lock(URI) :-
	mutex_unlock(URI).
release_write_lock(URI) :-
	mutex_unlock(URI).
with_read_lock(URI, Goal) :-
	with_mutex(URI, (exists_kb(URI),Goal)).
with_write_lock(URI, Goal) :-
	with_mutex(URI, (exists_kb(URI),Goal)).
