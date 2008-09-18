:- module(kb_manager, [
					new_kb/1, release_kb/1, add_axioms/2, run_query/3,
					default_kb/1, clear_kb/1,
					with_read_lock/2, with_write_lock/2
					]).

:- use_module(library(lists)).

:- use_module('../dl_translator/axioms_to_clauses', [axioms_to_clauses/6]).
:- use_module('../prolog_translator/abox_signature', [abox_signature/4]).
:- use_module('../prolog_translator/abox_translator', [assert_abox/2, write_abox/2]).
:- use_module('../prolog_translator/tbox_translator', [tbox2prolog/3]).
:- use_module(dlogger, [info/3, detail/3, warning/3]).
:- use_module(query, [query/4]).
:- use_module(config, [target/1, default_kb/1, kb_uri/2, 
				get_dlog_option/3, remove_dlog_options/1,
				abox_module_name/2, tbox_module_name/2, 
				abox_file_name/2, tbox_file_name/2]).


:- target(swi) -> 
	use_module(library(memfile)),
	use_module(library(listing), [portray_clause/1]),
	use_module(library(odbc), [odbc_free_statement/1, odbc_disconnect/1])
	; true.
:- target(sicstus) -> 
	use_module(core_sicstus_tools, [mutex_create/1, with_mutex/2, mutex_lock/1, mutex_unlock/1, setup_and_call_cleanup/3]),
	use_module(library(system), [delete_file/2]),
	use_module('../hash/dlog_hash', [])
	; true.


:- dynamic current_kb/1,
			kb_count/1.

:- volatile current_kb/1,
			kb_count/1.

:- initialization
		detail(kb_manager, initialization, 'KB manager initializing...'),
		assert(kb_count(1)), 
		default_kb(Def), 
		mutex_create(kb_count), 
		mutex_create(Def),
		assert(current_kb(Def)),
		info(kb_manager, initialization, 'KB manager initialized.').


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
	assert(current_kb(URI)),
	info(kb_manager, new_kb(URI), 'New KB created.').

release_kb(URI) :-
	with_write_lock(URI,
	(
		clear_kb(URI),
		remove_dlog_options(URI), %remove KB-specific options
		retract(current_kb(URI))
	)),
	%,mutex_destroy(URI) %TODO
	info(kb_manager, release_kb(URI), 'KB released.').

clear_kb(URI) :- 
	with_write_lock(URI,
	(
		tbox_module_name(URI, TB),
		abox_module_name(URI, AB),
		close_DB_connections(AB),
		abolish_module(AB),
		abolish_module(TB)
		%, remove_dlog_options(URI) %TODO: remove options? -> only for default kb?
	)),
	info(kb_manager, clear_kb(URI), 'KB cleared.').

close_DB_connections(AB) :-
	info(kb_manager, close_DB_connections(AB), 'Closing DB connections'),
	current_predicate(AB:'$dlog_active_statement'/1),
	AB:'$dlog_active_statement'(_, Statement),
	catch(
		odbc_free_statement(Statement),
		error(existence_error(odbc_statement_handle, _), _),
		fail),
	fail.
close_DB_connections(AB) :-
	current_predicate(AB:'$dlog_open_DB_connection'/1),
	AB:'$dlog_open_DB_connection'(Connection),
	catch(
		odbc_disconnect(Connection),
		error(existence_error(odbc_connection, _), _), 
		fail),
	fail. %TODO: Debug: odbc_statistics(statements(Created, Freed))
close_DB_connections(_).
	

abolish_module(Module) :-
	current_predicate(Module:P),
	%\+ predicate_property(AB:AP, built_in),
	%\+ predicate_property(AB:AP, imported_from(_Module)),
	%\+ predicate_property(AB:AP, transparent),
	%what can't be removed, stayes there
	catch(abolish(Module:P), error(permission_error(_,_,_),_), fail),
	fail.
abolish_module(_Module).

add_axioms(URI, axioms(ImpliesCL, ImpliesRL, TransL, ABox, Concepts, Roles, DBConnections, DBPredicates)) :- %TODO: eltárolni, hozzáadni
	info(kb_manager, add_axioms(URI, ...), 'Adding axioms to KB.'), %TODO: Concepts, Roles
	detail(kb_manager, add_axioms(URI, axioms(ImpliesCL, ImpliesRL, TransL, ABox, Concepts, Roles, DBConnections, DBPredicates)), 'Axioms:'),
	exists_kb(URI),
	axioms_to_clauses(URI, [ImpliesCL, ImpliesRL, TransL],
			  TBox_Clauses, IBox, HBox, _), %TODO!
	detail(kb_manager, add_axioms(URI, ...), 'Clauses ready.'),
	abox_signature(ABox, DBPredicates, ABoxData, Signature),
	detail(kb_manager, (add_axioms(URI, ...) -> Signature), 'ABox signature: '),
	get_dlog_option(abox_target, URI, ATarget),
	get_dlog_option(tbox_target, URI, TTarget),
	with_write_lock(URI, 
	(	
		add_abox(ATarget, URI, abox(ABoxData, DBConnections, DBPredicates)),
		detail(kb_manager, add_axioms(URI, ...), 'ABox done.'),
		add_tbox(TTarget, URI, tbox(TBox_Clauses, IBox, HBox), abox(Signature))
	)),
	info(kb_manager, add_axioms(URI, ...), 'Axioms added to KB.').


add_abox(assert, URI, ABox) :- !,
	assert_abox(URI, ABox). 
add_abox(allinonefile, URI, ABox) :-
	current_output(Out),
	abox_file_name(URI, FileName),
	setup_and_call_cleanup( 
		open(FileName, write, Stream),
		setup_and_call_cleanup(
			set_output(Stream),
			write_abox(URI, ABox), %TODO: once?
			set_output(Out)
		),
		close(Stream)	
	),
	load_files(FileName, []).
%%add_abox(Target, URI, ABox) :-
add_abox(tempfile, URI, ABox) :-
	setup_and_call_cleanup(
		( %setup file
			abox_file_name(URI, FileName),
			(	%%Target == tempfile,
				target(swi)
			->	new_memory_file(MemFile)
			;	true
			)
		),
		( %call (main body)
			current_output(Out),
			setup_and_call_cleanup(
				( %setup output stream
					(	%%Target == tempfile,
						target(swi)
					->	open_memory_file(MemFile, write, Stream)
					;	open(FileName, write, Stream)
					)
				),
				setup_and_call_cleanup( % write ABox
					set_output(Stream),
					write_abox(URI, ABox),
					set_output(Out)
				), 
				close(Stream)
			),
			(	%load ABox
				%%Target == tempfile,
				target(swi)
			->	%abox_module_name(URI, AB),
				setup_and_call_cleanup(
					open_memory_file(MemFile, read, Stream2),
					load_files(FileName, [stream(Stream2)]), 
					close(Stream2)
				)
			;	load_files(FileName, [])
			)
		),
		( %cleanup temp file
			%%(	Target == tempfile
			%%->	
				(	target(swi)
				->	free_memory_file(MemFile)
				;	delete_file(FileName, [])
				)
			%%;	true
			%%)
		)
	).

add_tbox(assert, URI, TBox, ABox) :- !,
	%throw(not_implemented), %TODO
	tbox2prolog(URI, TBox, ABox). %TODO: finalize dynamic? (SWI)
add_tbox(allinonefile, URI, TBox, ABox) :-
	current_output(Out),
	tbox_file_name(URI, FileName),
	setup_and_call_cleanup( 
		open(FileName, write, Stream),
		setup_and_call_cleanup(
			set_output(Stream),
			tbox2prolog(URI, TBox, ABox), 
			set_output(Out)
		),
		close(Stream)	
	),
	load_files(FileName, []).
%%add_tbox(Target, URI, TBox, ABox) :-
add_tbox(tempfile, URI, TBox, ABox) :-
	setup_and_call_cleanup(
		( %setup file
			tbox_file_name(URI, FileName),
			(	%%Target == tempfile,
				target(swi)
			->	new_memory_file(MemFile)
			;	true
			)
		),
		( %call (main body)
			current_output(Out),
			setup_and_call_cleanup(
				( %setup output stream
					(	%%Target == tempfile,
						target(swi)
					->	open_memory_file(MemFile, write, Stream)
					;	open(FileName, write, Stream)
					)
				),
				setup_and_call_cleanup( % write TBox
					set_output(Stream),
					tbox2prolog(URI, TBox, ABox),
					set_output(Out)
				), 
				close(Stream)
			),
			(	%load TBox
				%%Target == tempfile,
				target(swi)
			->	%tbox_module_name(URI, TB),
				setup_and_call_cleanup(
					open_memory_file(MemFile, read, Stream2),
					load_files(FileName, [stream(Stream2)]), 
					close(Stream2)
				)
			;	load_files(FileName, [])
			)
		),
		( %cleanup temp file
			%%(	Target == tempfile
			%%->	
				(	target(swi)
				->	free_memory_file(MemFile)
				;	delete_file(FileName, [])
				)
			%%;	true
			%%)
		)
	).


run_query(URI, Query, Answer) :- 
	info(kb_manager, run_query(URI, ...), 'Querying KB.'),
	detail(kb_manager, run_query(URI, Query, ...), 'Query:'),
	with_read_lock(URI,
	(
		tbox_module_name(URI, TBox),
		abox_module_name(URI, ABox),
		query(Query, TBox, ABox, Answer)
	)),
	detail(kb_manager, run_query(URI, Query, Answer), 'Query results:').



get_read_lock(URI) :-
	exists_kb(URI), %don't create mutex if not exists
	mutex_lock(URI),
	exists_kb(URI). %check if still existing
get_write_lock(URI) :-
	exists_kb(URI), 
	mutex_lock(URI),
	exists_kb(URI).
release_read_lock(URI) :-
	mutex_unlock(URI).
release_write_lock(URI) :-
	mutex_unlock(URI).
with_read_lock(URI, Goal) :-
	exists_kb(URI), 
	with_mutex(URI, (exists_kb(URI),Goal)).
with_write_lock(URI, Goal) :-
	exists_kb(URI), 
	with_mutex(URI, (exists_kb(URI),Goal)).
