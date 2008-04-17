
:- module(dig_iface, [start_dig_server/0, stop_dig_server/0, execute_dig_file/2]).

:- use_module('../config', [target/1, get_dlog_option/2]).
:- use_module('../kb_manager').
:- use_module(dig_reader).
:- use_module(dig_identifier, [identifier/5]).
:- target(swi) -> 
		use_module(library('http/thread_httpd')),
		use_module(library('http/http_dispatch')), %hiba kezelés, több szolgáltatás ugyanazon a porton (OWL?)
		use_module(library('http/http_client')),
		use_module(library(sgml_write), [xml_write/3]),
		use_module(library(memfile))
		, use_module(library('http/http_error.pl'))	%debug modul -> 500 stacktrace-el
		; true.


%start_dig_server: start server on default port
start_dig_server :- 
	get_dlog_option(dig_server_port, Port),
	(
		target(swi)
	->
		start_dig_server_swi(Port)
	;
		throw(not_implemented) %TODO
	).
	

start_dig_server_swi(Port) :- %TODO átstruktúrálni az egészet
	get_dlog_option(dig_server_path, Path),
	get_dlog_option(dig_server_service_limit, Limit),
	http_handler(Path, swi_dig_server, [time_limit(Limit)]), 
	
	%TODO: ezt máshova...
	http_server(http_dispatch, [port(Port), timeout(30)]). %TODO: timeout(+SecondsOrInfinite): a kérésre mennyit várjon, 
											%workers(+N): hány szál (2)
											%after(:Goal) -> válasz után feldolgozás/statisztika
											%... (stack méret, ssl)


%stop_dig_server: stop server on default port
stop_dig_server :- 
	get_dlog_option(dig_server_port, Port),
	(
		target(swi)
	->
		stop_dig_server_swi(Port)
	;
		throw(not_implemented)
	).

stop_dig_server_swi(Port) :-
	http_stop_server(Port, _Options), !, %TODO: itt? TODO cut?!?
	http_current_handler(Path, swi_dig_server),
	http_delete_handler(Path).

swi_dig_server(Request) :-
	(
		memberchk(method(Method), Request),
		Method \== post 
	->  
		%throw(http_reply(cgi_stream(+Stream, +Len))) %?
		throw(http_reply(forbidden('anything with GET method'), ['Allow'('POST')])) %TODO
		%format('Allow: POST~n~n') %405 Method Not Allowed
	;	true
	),
	format('Content-type: text/xml~n~n', []),
	read_dig_from_request(Request, NS-DIG),
	(
		nonvar(DIG) 
	-> 
		catch(
			execute(DIG, Reply),
			no_such_kb,  
			Reply = no_such_kb
		),
		reply(Reply, NS)
	;	
		true
	).

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
				send_error(Code, Expl, Msg, NS)
			),
			close(DIGFile)
		),
		free_memory_file(MemFile)
	).

execute_dig_file(DIGFile, Reply) :-
	read_dig(DIGFile, _NS-DIG),
	execute(DIG, Reply).	


%execute(Command, Reply) throws no_such_kb
execute(newKB, kb(URI)) :- 
	new_kb(URI).
execute(clearKB(URI), kb_cleared(URI)) :- 
	clear_kb(URI).
execute(clearKBW(URI), kb_cleared_in_tells(URI)) :- 
	clear_kb(URI).
execute(releaseKB(URI), kb_released(URI)) :-
	release_kb(URI).
execute(getIdentifier, identifier).
execute(tells(URI, Axioms), Reply) :-
	add_axioms(URI, Axioms) ->
		Reply = axioms_added(URI, Axioms)
	;	Reply = failed_to_add_axiom(URI, Axioms).
execute(asks(URI, Asks), responses(Responses)) :-
	with_read_lock(URI, dig_iface:ask(Asks, URI, Responses)).

ask([], _, []).
ask([IDT-Type|Asks], URI, [Response|Responses]) :- 
	(
		IDT = id(ID) -> true
		; IDT = noid, ID = noid %TODO
	),
	catch(
		(
			Type = unknown(Elem) %unknown ask
		->
			Response = ID-unknown(Elem)
		;
			Type = unsupported(Elem) %unsupported concept
		->
			Response = ID-unsupported(Elem)
		;	
			unsupported(Type) %unsupported ask
		->
			Response = ID-unsupported(Elem)
		;
			( %supported question
				run_query(Type, URI, Answer)
			->
				Response = ID-Answer
			;
				Response = ID-error(bad_question(Type))  %bad answer
			)
		), 
		E, %whatever exception
		(
			E = no_such_kb -> throw(no_such_kb)
			;
			Response = ID-error(E)
		)
	),
	ask(Asks, URI, Responses).

%supported:
% allConceptNames
% allRoleNames
% allIndividuals
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
	%throw(http_reply(file('text/xml', 'interfaces/dig_identifier.dig'))). %resource? TODO NS
reply(axioms_added(_URI, _Axioms), NS) :-
	 send_xml(element(response, [xmlns=NS],	[element(ok, [],[])])).
reply(failed_to_add_axiom(_URI, _Axioms), NS) :-
	send_error(300, 'General Tell Error', 'add_axioms failed.', NS).
reply(responses(Responses), NS) :-
	create_responses(Responses, R),
	send_xml(element(responses, [xmlns=NS], R)).






