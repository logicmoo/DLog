
:- module(dig_iface, [start_dig_server/1, start_dig_server/0, stop_dig_server/1, stop_dig_server/0]).

:- use_module('../config', [target/1, get_dlog_option/2]).
:- use_module('../kb_manager').
:- use_module(dig_reader).
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
	start_dig_server(Port).
%start_dig_server(+Port): start server on specified port
start_dig_server(Port) :-
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
	stop_dig_server(Port). 
%stop_dig_server(+Port): stop server on specified port
stop_dig_server(Port) :-
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
	% print(user_error, 'Req:\n'),
	% print(user_error, Request),
	% print(user_error, '\n'),
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
	new_memory_file(MemFile),
	call_cleanup(
		(
			open_memory_file(MemFile, write, Stream),
			http_read_data(Request, _, [to(stream(Stream))]),
			close(Stream),
			open_memory_file(MemFile, read, DIGFile),
			catch(
				read_dig(stream(DIGFile), DIG),
				digerror(Code, Expl, Msg),
				send_error(Code, Expl, Msg)
			),
			close(DIGFile)
		),
		free_memory_file(MemFile)
	),
	% print(user_error, 'DIG:\n'),
	% print(user_error, DIG),
	% print(user_error, '\n'),
	(nonvar(DIG) -> execute(DIG)
	;	true).




send_error(Code, Expl, Msg) :-
	(atom(Msg) -> MsgA = Msg ; term_to_atom(Msg, MsgA)),
	send_xml(element(response, [xmlns='http://dl.kr.org/dig/2003/02/lang'], %TODO dig 1.0 ns
				[element(error, [code=Code, message=Expl],[MsgA])])).

send_xml(Element) :-
	current_output(Stream),
	xml_write(Stream, Element, []).

execute(newKB) :- 
	new_kb(URI),
	send_xml(element(response, [xmlns='http://dl.kr.org/dig/2003/02/lang'],
				[element(kb, [uri=URI],[])])).
execute(clearKB(URI)) :- 
	catch(
		(clear_kb(URI), 
		send_xml(element(response, [xmlns='http://dl.kr.org/dig/2003/02/lang'],
				[element(ok, [],[])]))),
		no_such_kb,  
		send_error(203, 'Unknown or stale KB URI', '')
	).
execute(clearKBW(URI)) :- 
	catch(
		(clear_kb(URI), 
		send_xml(element(response, [xmlns='http://dl.kr.org/dig/2003/02/lang'],
				[element(ok, [],[
						element(warning, [message='KB cleared'], ['Cleared KB in tells request.'])
						])
				]))
		),
		no_such_kb,  
		send_error(203, 'Unknown or stale KB URI', '')
	).
execute(releaseKB(URI)) :-
	catch(
		(release_kb(URI), 
		send_xml(element(response, [xmlns='http://dl.kr.org/dig/2003/02/lang'],
				[element(ok, [],[])]))),
		no_such_kb,  
		send_error(203, 'Unknown or stale KB URI', '')
	).
execute(getIdentifier) :-
	throw(http_reply(file('text/xml', 'interfaces/dig_identifier.dig'))). %resource?
execute(tells(URI, Axioms)) :-
	catch(
		(add_axioms(URI, Axioms) ->
			send_xml(element(response, [xmlns='http://dl.kr.org/dig/2003/02/lang'],	[element(ok, [],[])]))
		;	send_error(300, 'General Tell Error', 'add_axioms failed.')
		),
		no_such_kb,  
		send_error(203, 'Unknown or stale KB URI', '')
	).
execute(asks(URI, Asks)) :- 
		with_read_lock(URI, dig_iface:ask(Asks, URI, Responses)),
		send_xml(element(responses, [xmlns='http://dl.kr.org/dig/2003/02/lang'], Responses)).

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
			term_to_atom(Elem, ElemA),
			Response = element(error, [id=ID, code=402, message='Unknown Ask Operation'], [ElemA])
		;
			Type = unsupported(Elem) %unsupported concept
		->
			term_to_atom(Elem, ElemA),
			Response = element(error, [id=ID, code=401, message='Unsupported Ask Operation'], [ElemA])
		;	
			unsupported(Type) %unsupported ask
		->
			term_to_atom(Type, TypeA),
			Response = element(error, [id=ID, code=401, message='Unsupported Ask Operation'], [TypeA])
		;
			( %supported question
				run_query(Type, URI, Answer)
			->
				create_response(Answer, ID, Response)				
			;
				Response = element(error, [id=ID, code=400, message='General Ask Error'], []) %bad answer
			)
		), 
		E, %whatever exception
		(
			E = no_such_kb -> Response = element(error, [id=ID, code=203, message='Unknown or stale KB URI'], [''])
			;
			(term_to_atom(E, EA),
			Response = element(error, [id=ID, code=400, message='General Ask Error'], [EA]))
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

	

