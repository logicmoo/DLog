
:- module(dig_iface, [execute_dig_file/2]).

:- use_module('../core/config', [target/1, get_dlog_option/2]).
:- use_module('../core/kb_manager').
:- use_module('../core/dlogger', [error/3, warning/3, info/3, detail/3]).
:- use_module(dig_reader).
:- use_module(dig_identifier, [identifier/5]).
:- target(swi) -> 
		use_module(library('http/http_client')),
		use_module(library('http/http_dispatch')), 
		use_module(library(sgml_write), [xml_write/3]),
		use_module(library(memfile))
		; true.
:- target(sicstus) -> 
		use_module(dig_iface_sicstus_tools, [http_handler/3])
		; true.


%register the server
register_dig_server :- 
	get_dlog_option(dig_server_path, Path),
	get_dlog_option(dig_server_service_limit, Limit), %seconds or 'infinite'
	http_handler(Path, dig_server, [time_limit(Limit)]),
	info(dig_iface, register_dig_server, 'DIG server registered.').

%unregister the server
unregister_dig_server :- 
	http_current_handler(Path, dig_server),
	http_delete_handler(Path),
	info(dig_iface, register_dig_server, 'DIG server unregistered.').

:- target(swi) ->
	initialization(
		(info(dig_iface, initialization, 'DIG interface initializing...'),
		register_dig_server)) %TODO: option?
	;	true.

%service requests
dig_server(Request) :-
	info(dig_iface, dig_server(...), 'Request received.'),
	detail(dig_iface, dig_server(Request), 'Request parameters:'),
	(
		memberchk(method(Method), Request),
		Method \== post 
	->  
		%throw(http_reply(cgi_stream(+Stream, +Len))) %?
		warning(dig_iface, dig_server([method(Method), ...]), 'Method Not Allowed.'),
		throw(http_reply(forbidden('anything with GET method'), ['Allow'('POST')])) 
		%TODO: nicely formatted HTML with server details/statistics?
		%format('Allow: POST~n~n') %405 Method Not Allowed
	;	true
	),
	catch(
		(
			process_request(Request)
		->  true
		;   error(dig_iface, dig_server(...), 'Internal server error (processing failed).'),
			fail
		),
		E,
		(
			error(dig_iface, (dig_server(...) --> E), 'Internal server error (exception).'),
			throw(E)
		)
	).

process_request(Request) :-
	format('Content-type: text/xml~n~n', []),
	read_dig_from_request(Request, NS-DIG),
	detail(dig_iface, (process_request(...) --> NS-DIG), 'Request body:'),
	(
		nonvar(DIG) 
	-> 
		catch(
			execute(DIG, Reply),
			no_such_kb,  
			(	Reply = no_such_kb, 
				warning(dig_iface, process_request(...), 'Invalid KB reference.')
			)
		),
		reply(Reply, NS)
	;
		true %sent error
	).

%read the request from the HTTP stream
%TODO: memory limit? disk file?
read_dig_from_request(Request, NS-DIG) :-
	new_memory_file(MemFile),
	call_cleanup(
		(
			open_memory_file(MemFile, write, Stream),
			http_read_data(Request, _, [to(stream(Stream))]),
			close(Stream),
			open_memory_file(MemFile, read, DIGFile),
			catch(
				read_dig(stream(DIGFile), NS-DIG),
				digerror(Code, Expl, Msg, NS),
				(send_error(Code, Expl, Msg, NS),
				info(dig_iface, (read_dig_from_request(...) --> digerror(Code, Expl, Msg, NS)), 'Error in DIG code.') %TODO: info/warning?
				)
			),
			close(DIGFile)
		),
		free_memory_file(MemFile)
	).

%execute_dig_file(+DIGFile, -Reply): 
%Read commands from DIGFile, execute them, and return the result in Reply
execute_dig_file(DIGFile, Reply) :-
	read_dig(DIGFile, _NS-DIG),
	detail(dig_iface, (execute_dig_file(DIGFile, ...) --> DIG), 'DIG content:'),
	trace,execute(DIG, Reply).	


%execute(+Command, -Reply) throws no_such_kb
execute(newKB, kb(URI)) :- 
	info(dig_iface, execute(newKB, ...), 'Creating new KB.'),
	new_kb(URI).
execute(clearKB(URI), kb_cleared(URI)) :-
	(var(URI) -> default_kb(URI) ; true),
	info(dig_iface, execute(clearKB(URI), ...), 'Clearing KB.'),
	clear_kb(URI).
execute(clearKBW(URI), kb_cleared_in_tells(URI)) :- 
	(var(URI) -> default_kb(URI) ; true),
	info(dig_iface, execute(clearKB(URI), ...), 'Clearing KB (in a tell request!).'),
	clear_kb(URI).
execute(releaseKB(URI), kb_released(URI)) :-
	info(dig_iface, execute(releaseKB(URI), ...), 'Releasing KB.'),
	release_kb(URI).
execute(getIdentifier, identifier) :-
	info(dig_iface, execute(getIdentifier, ...), 'DIG identifier request.').
execute(tells(URI, Axioms), Reply) :-
	(var(URI) -> default_kb(URI) ; true),
	info(dig_iface, execute(tells(URI, ...), ...), 'Adding axioms.'),
	detail(dig_iface, execute(tells(URI, Axioms), ...), 'Axioms:'),
	(add_axioms(URI, Axioms) 
	->	Reply = axioms_added(URI, Axioms)
	;	Reply = failed_to_add_axiom(URI, Axioms),
		warning(dig_iface, execute(tells(URI, ...), ...), 'Failed to add axioms.') %TODO: warning/error?
	).
execute(asks(URI, Asks), responses(Responses)) :-
	(var(URI) -> default_kb(URI) ; true),
	info(dig_iface, execute(asks(URI, ...), ...), 'Running queries.'),
	with_read_lock(URI, dig_iface:ask(Asks, URI, Responses)).

%execute asks
ask([], _, []).
ask([IDT-Type|Asks], URI, [Response|Responses]) :- 
	(IDT = id(ID) 
	->	true
	;	IDT = noid, 
		ID = noid, %TODO
		warning(dig_iface, ask(..., URI, ...), 'Query ID missing.'),
		detail(dig_iface, ask([IDT-Type| ...], URI, ...), 'Query:')
	),
	catch(
		(
			Type = unknown(Elem) %unknown ask
		->
			Response = ID-unknown(Elem),
			info(dig_iface, ask([IDT-Type| ...], URI, ...), 'Unknown query.') %TODO: info/warning?
		;
			Type = unsupported(Elem) %unsupported concept
		->
			Response = ID-unsupported(Elem),
			info(dig_iface, ask([IDT-Type| ...], URI, ...), 'Unsupported concept in query.')
		;	
			unsupported(Type) %unsupported ask
		->
			Response = ID-unsupported(Elem),
			info(dig_iface, ask([IDT-Type| ...], URI, ...), 'Unsupported query operation.')
		;
			( %supported question
				run_query(URI, Type, Answer)
			->
				Response = ID-Answer,
				detail(dig_iface, ask([IDT-Type| ...], URI, [Response|...]), 'Query successful.')
			;
				Response = ID-error(bad_question(Type)),  %bad answer
				warning(dig_iface, ask([IDT-Type| ...], URI, ...), 'Query failed.')
			)
		), 
		E, %whatever exception
		(
			E == no_such_kb 
		->	throw(no_such_kb) %handled in process_request
		;	E == time_limit_exceeded
		->	%warning(dig_iface, ask([IDT-Type| ...], URI, ...), 'Timeout while running query.'),
			throw(time_limit_exceeded) %handled in dig_server
		;	E == '$aborted'
		->	throw('$aborted')
		;	Response = ID-error(E),
			warning(dig_iface, ask([IDT-Type| ...], URI, [Response|...]), 'Exception while running query.')
		)
	),
	ask(Asks, URI, Responses).

%supported:
unsupported(allConceptNames).
unsupported(allRoleNames).
unsupported(allIndividuals).
%instances(Concept)
%instance(Name, Concept)
%roleFillers(Name, Role)
%relatedIndividuals(Role)
unsupported(satisfiable(_Concept)). 
unsupported(subsumes(_Concept1, _Concept2)).
unsupported(disjoint(_Concept1, _Concept2)).
unsupported(parents(_Concept)).
unsupported(children(_Concept)).
unsupported(ancestors(_Concept)).
unsupported(descendants(_Concept)).
unsupported(equivalents(_Concept)).
unsupported(rparents(_Role)).
unsupported(rchildren(_Role)).
unsupported(rancestors(_Role)).
unsupported(rdescendants(_Role)).
unsupported(toldValues(_Name, _Attribute)).
unsupported(types(_Name)).


create_responses([], []).
create_responses([ID-R|Rs], [E|Es]) :-
	create_response(R, ID, E),
	create_responses(Rs, Es).

create_response(conceptSet(Synonyms), ID, element(conceptSet, [id=ID], Elems)) :-
	create_synonyms(Synonyms, Elems). 
create_response(roleSet(Synonyms), ID, element(roleSet, [id=ID], Elems)) :-
	create_synonyms(Synonyms, Elems).
create_response(individualSet(Individuals), ID, element(individualSet, [id=ID], Elems)) :-
	create_individuals(Individuals, Elems).
create_response(true, ID, element(true, [id=ID], [])).
create_response(false, ID, element(false, [id=ID], [])).
create_response(individualPairSet(Individual_pairs), ID, element(individualPairSet, [id=ID], Elems)) :-
	create_individual_pairs(Individual_pairs, Elems).
create_response(sval(Atom), ID, element(sval, [id=ID], [Atom])).
create_response(ival(Int), ID, element(ival, [id=ID], [Int])).

create_response(unknown(Elem), ID, element(error, [id=ID, code=402, message='Unknown Ask Operation'], [ElemA]) ) :-
	term_to_atom(Elem, ElemA).
create_response(unsupported(Elem), ID, element(error, [id=ID, code=401, message='Unsupported Ask Operation'], [ElemA])) :-
	term_to_atom(Elem, ElemA).
%create_response(error, ID, element(error, [id=ID, code=400, message='General Ask Error'], [])).
create_response(error(E), ID, element(error, [id=ID, code=400, message='General Ask Error'], [EA])) :-
	term_to_atom(E, EA).


create_individuals([], []).
create_individuals([Individual|Individuals], [element(individual, [name=Individual],[])| Elems]) :-
	create_individuals(Individuals, Elems).
	
create_individual_pairs([], []).
create_individual_pairs([I1-I2|Individual_pairs], 
	[element(individualPair, [],
		[element(individual, [name=I1],[]),
		 element(individual, [name=I2],[])]) | Elems]) :-
	create_individual_pairs(Individual_pairs, Elems).

create_synonyms([], []).
create_synonyms([Concepts|Synonyms], [element(synonyms, [], ConceptElems)|Elems]) :-
	create_concepts(Concepts, ConceptElems),
	create_synonyms(Synonyms, Elems).

create_concepts([], []).
create_concepts([Concept|Concepts], [Elem|Elems]) :-
	create_concept(Concept, Elem),
	create_concepts(Concepts, Elems).

create_concept(aconcept(Concept), element(catom, [name=Concept], [])).
create_concept(arole(Role), element(ratom, [name=Role], [])). %TODO: feature, attribute?



send_xml(Element) :-
	current_output(Stream),
	xml_write(Stream, Element, []).

send_error(Code, Expl, Msg, NS) :-
	(atom(Msg) -> MsgA = Msg ; term_to_atom(Msg, MsgA)),
	send_xml(element(response, [xmlns=NS],
				[element(error, [code=Code, message=Expl],[MsgA])])).


reply(kb(URI), NS) :-
	send_xml(element(response, [xmlns=NS], [element(kb, [uri=URI],[])])).
reply(kb_cleared(_URI), NS) :-
		send_xml(element(response, [xmlns=NS], [element(ok, [],[])])).
reply(kb_cleared_in_tells(_URI), NS) :-
	send_xml(element(response, [xmlns=NS], [element(ok, [],[
			element(warning, [message='KB cleared'], ['Cleared KB in tells request.'])
		])])).
reply(no_such_kb, NS) :-
	send_error(203, 'Unknown or stale KB URI', '', NS).
reply(kb_released(_URI), NS) :-
	send_xml(element(response, [xmlns=NS], [element(ok, [],[])])).
reply(identifier, NS) :-
	get_dlog_option(name, Name),
	get_dlog_option(version, Version),
	get_dlog_option(description, Message),
	identifier(NS, Name, Version, Message, Identifier),
	send_xml(Identifier).
reply(axioms_added(_URI, _Axioms), NS) :-
	 send_xml(element(response, [xmlns=NS],	[element(ok, [],[])])).
reply(failed_to_add_axiom(_URI, _Axioms), NS) :-
	send_error(300, 'General Tell Error', 'add_axioms failed.', NS).
reply(responses(Responses), NS) :-
	create_responses(Responses, R),
	send_xml(element(responses, [xmlns=NS], R)).




