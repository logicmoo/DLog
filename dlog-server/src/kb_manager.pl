:- module(kb_manager, [
					new_kb/1, release_kb/1, add_axioms/2, run_query/3, %addTAxioms/2, addAAxioms/2 ?
					default_kb/1, clean_default_kb/0,
					with_read_lock/2, with_write_lock/2
					]).

:- use_module(library(lists), [append/3]).
:- use_module('dl_translator/translator', [axioms_to_clauses/5]). %TODO
:- use_module('prolog_translator/abox_signature', [abox_signature/3]).
:- use_module('prolog_translator/abox_translator', [abox2prolog/3]).
:- use_module('prolog_translator/tbox_translator', [tbox2prolog/4]).
:- use_module(query, [query/4]).


:- dynamic current_kb/1,
			kb_count/1.

default_kb('dlog://0').

:- assert(kb_count(1)), mutex_create(kb_count), default_kb(Def), mutex_create(Def). %initialization?




exists_kb(URI) :-
	nonvar(URI),
	current_kb(URI) -> true
	; throw(no_such_kb).

new_kb(URI) :- 
	%TODO dig 1.0 URI, %{http://|dlog://}[<server>:<port>/]{<sorszám>|<UUID>}
	with_mutex(kb_count,
	(
		retract(kb_count(ID)),
		ID1 is ID+1,
		assert(kb_count(ID1))
	)),
	atom_concat('dlog://', ID, URI),
	mutex_create(URI),
	assert(current_kb(URI)).

release_kb(URI) :-
	exists_kb(URI),
	tbox_module(URI, TB),
	abox_module(URI, AB),
	with_write_lock(URI,
	(
		retractall(AB:_), %TODO: file esetén törlés
		retractall(TB:_),	%TODO: beállítások törlése
		retract(current_kb(URI))
	))
	%,mutex_destroy(URI) %TODO
	. 

add_axioms(URI, axioms(ImpliesCL, ImpliesRL, TransL, ABox)) :- %TODO: eltárolni, hozzáadni
	exists_kb(URI),
	% append(ImpliesCL, ImpliesRL, TBox0), 
	% append(TBox0, TransL, TBox), %TODO
	axioms_to_clauses([ImpliesCL, ImpliesRL, TransL], TBox_Clauses, IBox,HBox,_), %TODO
	abox_signature(ABox, ABoxStr, Signature),
	
	tbox_module(URI, TB),
	abox_module(URI, AB),
	with_write_lock(URI,
	(	
		% indexing(yes) : [yes, no] whether to generate inverses for roles for efficient indexes
		% generate_abox(no): [yes, no] whether to generate an ABox Prolog file
		abox2prolog(AB, abox(ABoxStr), []), %TODO megadott modulba assert
		tbox2prolog(TB, tbox(TBox_Clauses, IBox, HBox), abox(Signature), _Options)
	)).

run_query(Query, URI, Answer) :- 
	exists_kb(URI),
	tbox_module(URI, TBox),
	abox_module(URI, ABox),
	with_read_lock(URI,
	(
		query(Query, TBox, ABox, Answer)
	)).


clean_default_kb :- 
	tbox_module('dlog://0', TB),
	abox_module('dlog://0', AB),
	with_write_lock('dlog://0',
	(
		retract_all(AB:_), %TODO: file esetén törlés
		retract_all(TB:_)	%TODO: beállítások törlése
	)).


%tbox_module(+URI, -Module): URI-hoz tartozó TBox modul neve
tbox_module(URI, Module) :- 
	atom_concat('dlog://', N, URI),
	atom_concat('tbox', N, Module).
abox_module(URI, Module) :- 
	atom_concat('dlog://', N, URI),
	atom_concat('abox', N, Module).
tbox_file(URI, File) :- 
	atom_concat('dlog://', N, URI),
	atom_concat('../output/tbox', N, F),
	atom_concat(F, '.pl', File).
abox_file(URI, File) :- 
	atom_concat('dlog://', N, URI),
	atom_concat('../output/abox', N, F),
	atom_concat(F, '.pl', File).

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
