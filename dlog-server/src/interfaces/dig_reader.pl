
:- module(dig_reader,[read_dig/2]).

:- use_module('../core/config', [target/1, get_dlog_option/2]).
%:- use_module('../core/kb_manager', [default_kb/1]). %TODO ezt ne itt
:- target(sicstus) -> 
        use_module('xml_reader/xml_parser') %TODO
        ; true.
:- target(swi) ->
         use_module(library(sgml))
	 ; true.
:- use_module(library(lists)).
%:- use_module('../tools/show').


% read_dig(+DIG_Input,-Answer) throws 
%			digerror(102, 'Malformed Request (XML error)', Message, NS),
%			digerror(302, 'Unknown Tell Operation', E, NS),
%			digerror(301, 'Unsupported Tell Operation', Elem, NS),
%			digerror(103, 'Unsupported Operation', Elem, NS) -- csak tells
% beolvassa a DIG_Input DIG file tartalmát
% Answer: 
%	tells(ID, (ImpliesCL, InvRL, ImpliesRL, TransL, ABox))
%	asks(ID, (...))
%	newKB
%	releaseKB(ID)
%	getIdentifier
read_dig(DIG_Input, NS-Answer) :-
	(
	load_structure(DIG_Input, [element(dig:Root,Atts,Elems)], 
	[
		dialect(xmlns),
		space(remove),
		%number(integer),
		qualify_attributes(true),
		call(urlns, dig_reader:xmlns), 
		call(error, dig_reader:xmlerror)
	]) %TODO: még elfogad 2 gyökér elemet, és idézöjel nélküli attribútumot
	-> true
	; throw(digerror(102, 'Malformed Request (XML error)', '', 
		'http://dl.kr.org/dig/2003/02/lang')) %no way to decide -> dig 1.1
	),
	memberchk((dig:xmlns=NS), Atts),
	%assertion(NS=='http://dl.kr.org/dig/2003/02/lang' 
	%			; NS == 'http://dl.kr.org/dig/lang'),
	catch(
		parse(Root, Atts, Elems, Answer),
		digerror(Code, Expl, Msg),
		throw(digerror(Code, Expl, Msg, NS))
	).

xmlns('http://dl.kr.org/dig/2003/02/lang', dig, _). %dig 1.1
xmlns('http://dl.kr.org/dig/lang', dig, _). %dig 1.0

xmlerror(_Severity, Message, _Parser) :- 
	throw(digerror(102, 'Malformed Request (XML error)', Message)).

%parse(+Root, +Atts, +Elems, -Answer).
parse(newKB, _Atts, _Elems, newKB) :- !.
parse(clearKB, Atts, _Elems, clearKB(URI)) :- !,
	(memberchk((dig:uri=URI),Atts) -> true
	; true). % uninstantiated
	%; default_kb(URI)). %dig1.0
parse(releaseKB, Atts, _Elems, releaseKB(URI)) :- !,
	memberchk(dig:uri=URI,Atts).
parse(getIdentifier, _Atts, _Elems, getIdentifier).
parse(tells, Atts, Elems, Req) :- !,
	(memberchk((dig:uri=URI),Atts) -> true
	; true), % uninstantiated
	%; default_kb(URI)), %dig 1.0
	catch(
		(phrase(parse_tells(Elems), axioms([], [], [], []), Axioms), Req = tells(URI, Axioms)),
		clearKB,
		Req = clearKBW(URI)
	).
parse(asks, Atts, Elems, asks(URI, Asks)) :- !,
	(memberchk((dig:uri=URI),Atts) -> true
	; true), % uninstantiated
	%; default_kb(URI)), %dig 1.0
	phrase(parse_asks(Elems), Asks).
parse(Req, Atts, Elems, _) :- 
	throw(digerror(101, 'Unknown Request', element(dig:Req, Atts, Elems))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           TELLS                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% parse_tell(+DIG_Axioms, +axioms(ImpliesCL0, ImpliesRL0, TransL0, ABox0), -axioms(ImpliesCL, ImpliesRL, TransL, ABox)):
%	1. lista: fogalomtartalmazasi axiomak:
%	   implies(C1,C2), ahol C1 es C2 szerepek
%	2. lista: szereptartalmazasi axiomak:
%	   subrole(R, S), ahol R es S szerepek
%	3. lista: tranzitiv szerepek
%	   R, azaz egyszeruen szerepek listaja
%
%	Egy r szerep szintaxisa: arole(r) --> arole(r, i1, i2) --> inv(r)? TODO: feature, attribute
%	Egy c atomi fogalom: aconcept(c) --> aconcept(c, i1)
%	Negalt fogalom: not(c)
%	sok fogalom metszete: and([C1,C2,...Ck])
%	sok fogalom unioja: or([C1,C2,...Ck])
%	minden R-kovetoje C: all(R,C)
%	letezik R-kovetoje, aki C: some(R,C)
%	legalabb N R-kovetoje C: atleast(N,R,C)
%	legfeljebb N R-kovetoje C: atmost(N,R,C)
%	top, bottom
%
parse_tells([]) --> [].
parse_tells([element(dig:AxiomType, Atts, Elems)|Axioms]) -->
	parse_tell(AxiomType, Atts, Elems), !,
	parse_tells(Axioms).
parse_tells([E|Axioms]) --> 
	{get_dlog_option(dig_reader_fault_tolerance, drop) -> true
	; throw(digerror(302, 'Unknown Tell Operation', E))},
	parse_tells(Axioms).


%parse_tell(+Axiom, +Atts, +Elems, +axioms(ImpliesCL, ImpliesRL, TransL, ABox), -axioms(ImpliesCL1, ImpliesRL1, TransL1, ABox1)):
%
%Concept axioms
parse_tell(impliesc, _Atts, 
			[element(dig:ConceptType1, Atts1, Elems1), 
			 element(dig:ConceptType2, Atts2, Elems2)]) -->
	{parse_concept(ConceptType1, Atts1, Elems1, Concept1), 
	parse_concept(ConceptType2, Atts2, Elems2, Concept2)},
	add_impliesc(Concept1, Concept2).

parse_tell(equalc, _Atts, 
			[element(dig:ConceptType1, Atts1, Elems1), 
			 element(dig:ConceptType2, Atts2, Elems2)]) -->
	{parse_concept(ConceptType1, Atts1, Elems1, Concept1), 
	parse_concept(ConceptType2, Atts2, Elems2, Concept2)}, 
	add_impliesc(Concept1, Concept2),
	add_impliesc(Concept2, Concept1).

parse_tell(disjoint, _Atts, Elems) -->
	{parse_concepts(Elems, Concepts)},
	add_disjoints(Concepts).


%Role axioms
parse_tell(impliesr, _Atts, 
			[element(dig:RoleType1, Atts1, Elems1), 
			 element(dig:RoleType2, Atts2, Elems2)]) -->
	{parse_role(RoleType1, Atts1, Elems1, Role1), 
	parse_role(RoleType2, Atts2, Elems2, Role2)}, 
	add_impliesr(Role1, Role2).

parse_tell(equalr, _Atts, 
			[element(dig:RoleType1, Atts1, Elems1), 
			 element(dig:RoleType2, Atts2, Elems2)]) -->
	{parse_role(RoleType1, Atts1, Elems1, Role1), 
	parse_role(RoleType2, Atts2, Elems2, Role2)}, 
	add_impliesr(Role1, Role2),
	add_impliesr(Role2, Role1).

parse_tell(domain, _Atts, 
			[element(dig:RoleType, AttsR, ElemsR), 
			 element(dig:ConceptType, AttsC, ElemsC)]) -->
	{parse_role(RoleType, AttsR, ElemsR, Role), 
	parse_concept(ConceptType, AttsC, ElemsC, Concept)}, 
	add_impliesc(some(Role, top), Concept).

parse_tell(range, _Atts, 
			[element(dig:RoleType, AttsR, ElemsR), 
			 element(dig:ConceptType, AttsC, ElemsC)]) -->
	{parse_role(RoleType, AttsR, ElemsR, Role), 
	parse_concept(ConceptType, AttsC, ElemsC, Concept)}, 
	%add_impliesc(some(inv(Role), top), Concept).
	add_impliesc(top, all(Role, Concept)).

parse_tell(rangeint, Atts,
			[element(dig:AttributeType, AttsA, ElemsA)]) -->
	{parse_attribute(AttributeType, AttsA, ElemsA, Role)},  
	add_impliesc(top, all(Role, domain(int))), %SPEC
	{throw_tell_error(element(rangeint , Atts, [element(dig:AttributeType, AttsA, ElemsA)]))}.

parse_tell(rangestring, Atts,
			[element(dig:AttributeType, AttsA, ElemsA)]) -->
	{parse_attribute(AttributeType, AttsA, ElemsA, Role)},  
	add_impliesc(top, all(Role, domain(string))), %SPEC
	{throw_tell_error(element(rangestring , Atts, [element(dig:AttributeType, AttsA, ElemsA)]))}.

parse_tell(transitive, _Atts, 
			[element(dig:RoleType, AttsR, ElemsR)]) -->
	{parse_role(RoleType, AttsR, ElemsR, Role)},
	add_transitive(Role). %TODO: inverz?, functional, feature, attribute? ->(300, 'General Tell Error'), vagy fail?

parse_tell(functional, _Atts, 
			[element(dig:RoleType, AttsR, ElemsR)]) -->
	{parse_role(RoleType, AttsR, ElemsR, Role)}, 
	add_impliesc(top, atmost(1, Role, top)).


%ABox axioms
parse_tell(instanceof, _Atts, 
			[element(dig:individual, AttsI, _ElemsI), 
			 element(dig:ConceptType, AttsC, ElemsC)]) -->
	{parse_concept(ConceptType, AttsC, ElemsC, Concept),
	memberchk((dig:name=I1), AttsI)}, 
	add_ABox(aconcept(Concept, I1)).

parse_tell(related, _Atts, 
			[element(dig:individual, AttsI1, _ElemsI1), 
			 element(dig:RoleType, AttsR, ElemsR), 
			 element(dig:individual, AttsI2, _ElemsI2)]) --> 
	{parse_role(RoleType, AttsR, ElemsR, Role),
	memberchk((dig:name=I1), AttsI1),
	memberchk((dig:name=I2), AttsI2)}, 
	add_ABox(arole(Role, I1, I2)).

parse_tell(value, Atts, 
			[element(dig:individual, AttsI, ElemsI), 
			 element(dig:AttributeType, AttsA, ElemsA), 
			 element(dig:ValueType, AttsV, [ElemsV])]) -->
	{parse_attribute(AttributeType, AttsA, ElemsA, Attribute),
	memberchk((dig:name=I), AttsI), 
	(
		ValueType == sval -> Value = ElemsV %SPEC
	;	ValueType == ival, atom_codes(ElemsV,NC), number_codes(Value,NC)
	)}, 
	add_ABox(avalue(I, Attribute, Value)), %SPEC
	{throw_tell_error(element(dig:value, Atts, [element(dig:individual, AttsI, ElemsI), 
							  element(dig:AttributeType, AttsA, ElemsA), 
							  element(dig:ValueType, AttsV, [ElemsV])]))}.


parse_tell(defconcept, _Atts, _Elems) --> []. %TODO ezekkel valamit?
parse_tell(defrole, _Atts, _Elems) --> [].
parse_tell(deffeature, Atts, _Elems) --> 
	{memberchk((dig:name=Role), Atts)},
	add_impliesc(top, atmost(1, arole(Role), top)).
parse_tell(defattribute , Atts, Elems) --> %=?= int/string functional role
	{throw_tell_error(element(dig:defattribute , Atts, Elems))}.
parse_tell(defindividual , _Atts, _Elems) --> [].

parse_tell(clearKB , _Atts, _Elems) --> {throw(clearKB)}.


throw_tell_error(Elem) :- 
	get_dlog_option(dig_reader_fault_tolerance, yes) -> true
	; get_dlog_option(dig_reader_fault_tolerance, drop) -> fail
	; throw(digerror(301, 'Unsupported Tell Operation', Elem)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Add Tell Axioms %%%%%%%%%%%%%%%%%%%
add_impliesc(Concept1, Concept2,
			axioms(ImpliesCL, ImpliesRL, TransL, ABox),
			axioms([implies(Concept1, Concept2)| ImpliesCL], ImpliesRL, TransL, ABox)).
add_disjoints([]) --> [].
add_disjoints([_]) --> [].
add_disjoints([Concept, D|Disjoints]) --> %TODO ok? 1 elemü or()?; 0 elemü or/and
	add_impliesc(and([Concept, or([D|Disjoints])]), bottom),
	add_disjoints([D|Disjoints]).
add_impliesr(Role1, Role2,
			axioms(ImpliesCL, ImpliesRL, TransL, ABox),
			axioms(ImpliesCL, [subrole(Role1, Role2)| ImpliesRL], TransL, ABox)).
add_transitive(Role,
			axioms(ImpliesCL, ImpliesRL, TransL, ABox),
			axioms(ImpliesCL, ImpliesRL, [Role| TransL], ABox)).
add_ABox(Axiom,
		axioms(ImpliesCL, ImpliesRL, TransL, ABox),
		axioms(ImpliesCL, ImpliesRL, TransL, [Axiom|ABox])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           ASKS                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%parse_asks(+Elems, +Asks0, -Asks): 
parse_asks([]) --> [].
parse_asks([element(dig:AskType, Atts, Elems)|Asks], State0, State) :- %exception miatt randa...
	catch(	
		parse_ask(AskType, Atts, Elems, State0, State1), 
		digerror(103, _, _), %nem támogatott concept
		(
			( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
			State1 = [ID-unsupported(element(dig:AskType, Atts, Elems)) | State0]
		)
	),  !,
	parse_asks(Asks, State1, State).
parse_asks([A|Asks]) --> 
	{( 
		A = element(dig:_AskType, Atts, _Elems), memberchk((dig:id=ID0), Atts) 
		-> ID=id(ID0) 
		; ID=noid %TODO
	)},
	[ID-unknown(A)],
	parse_asks(Asks).



parse_ask(allConceptNames, Atts, _Elems) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid )}, %TODO ha nincs ID, hiba?
	[ID-allConceptNames]. %SPEC
parse_ask(allRoleNames, Atts, _Elems) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid )},
	[ID-allRoleNames].
parse_ask(allIndividuals, Atts, _Elems) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid )},
	[ID-allIndividuals].
parse_ask(satisfiable, Atts, 
		[element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)},	
	[ID-satisfiable(Concept)].
parse_ask(subsumes, Atts, 
		[element(dig:ConceptType1, AttsC1, ElemsC1),
		 element(dig:ConceptType2, AttsC2, ElemsC2)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType1, AttsC1, ElemsC1, Concept1),
	parse_concept(ConceptType2, AttsC2, ElemsC2, Concept2)},	
	[ID-subsumes(Concept1, Concept2)].
parse_ask(disjoint, Atts, 
		[element(dig:ConceptType1, AttsC1, ElemsC1),
		 element(dig:ConceptType2, AttsC2, ElemsC2)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType1, AttsC1, ElemsC1, Concept1),
	parse_concept(ConceptType2, AttsC2, ElemsC2, Concept2)},	
	[ID-disjoint(Concept1, Concept2)].
parse_ask(parents, Atts, 
		[element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)},	
	[ID-parents(Concept)].
parse_ask(children, Atts, 
		[element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)},	
	[ID-children(Concept)].
parse_ask(ancestors, Atts, 
		[element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)},	
	[ID-ancestors(Concept)].
parse_ask(descendants, Atts, 
		[element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)},	
	[ID-descendants(Concept)].
parse_ask(equivalents, Atts, 
		[element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)},	
	[ID-equivalents(Concept)].

parse_ask(rparents, Atts, 
		[element(dig:RoleType, AttsR, ElemsR)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_role(RoleType, AttsR, ElemsR, Role)},	
	[ID-rparents(Role)].
parse_ask(rchildren, Atts, 
		[element(dig:RoleType, AttsR, ElemsR)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_role(RoleType, AttsR, ElemsR, Role)},	
	[ID-rchildren(Role)].
parse_ask(rancestors, Atts, 
		[element(dig:RoleType, AttsR, ElemsR)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_role(RoleType, AttsR, ElemsR, Role)},	
	[ID-rancestors(Role)].
parse_ask(rdescendants, Atts, 
		[element(dig:RoleType, AttsR, ElemsR)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_role(RoleType, AttsR, ElemsR, Role)}, 
	[ID-rdescendants(Role)].

parse_ask(instances, Atts, 
		[element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)}, 
	[ID-instances(Concept)].
parse_ask(instance, Atts, 
		[element(dig:individual, AttsI, _ElemsI),
		 element(dig:ConceptType, AttsC, ElemsC)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	memberchk((dig:name=Name), AttsI),
	parse_concept(ConceptType, AttsC, ElemsC, Concept)}, 
	[ID-instance(Name, Concept)].
parse_ask(roleFillers, Atts, 
		[element(dig:individual, AttsI, _ElemsI),
		 element(dig:RoleType, AttsR, ElemsR)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	memberchk((dig:name=Name), AttsI),
	parse_role(RoleType, AttsR, ElemsR, Role)}, 
	[ID-roleFillers(Name, Role)].
parse_ask(relatedIndividuals, Atts, 
		[element(dig:RoleType, AttsR, ElemsR)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	parse_role(RoleType, AttsR, ElemsR, Role)}, 
	[ID-relatedIndividuals(Role)].
parse_ask(toldValues, Atts, 
		[element(dig:individual, AttsI, _ElemsI),
		 element(dig:AttributeType, AttsA, ElemsA)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	memberchk((dig:name=Name), AttsI),
	parse_attribute(AttributeType, AttsA, ElemsA, Attribute)}, 
	[ID-toldValues(Name, Attribute)].
parse_ask(types, Atts, 
		[element(dig:individual, AttsI, _ElemsI)]) --> 
	{( memberchk((dig:id=ID0), Atts) -> ID=id(ID0) ; ID=noid ),
	memberchk((dig:name=Name), AttsI)}, 
	[ID-types(Name)].



%throw_ask_error(Elem) :- throw(digerror(401, 'Unsupported Ask Operation', Elem)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Concept Language                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%parse_role(+RoleType, +Atts, +Elems, -Role)
parse_role(ratom, Atts, _Elems, arole(Role)) :- 
	memberchk((dig:name=Role), Atts), !.
parse_role(feature, Atts, _Elems, arole(Feature)) :- 
	memberchk((dig:name=Feature), Atts), !.
parse_role(inverse, _Atts, [element(dig:RoleType, Atts, Elems)], Role) :- 
	% Role = inv(Role1), %TODO attribute, chain?
	% parse_role(RoleType, Atts, Elems, Role1), !.
	(
		RoleType = inverse -> 
		Elems = [element(dig:RoleType1, Atts1, Elems1)], %inv(inv()) eliminálása
		parse_role(RoleType1, Atts1, Elems1, Role)
	;
		Role = inv(Role1),
		parse_role(RoleType, Atts, Elems, Role1)
	), !.
parse_role(AttributeType, Atts, Elems, Attribute) :- 
	parse_attribute(AttributeType, Atts, Elems, Attribute), !, 
	throw_concept_error(element(dig:AttributeType, Atts, Elems)).

%parse_attribute(+AttributeType, +Atts, +Elems, -Attribute)	
parse_attribute(attribute, Atts, _Elems, arole(Attribute)) :-
	memberchk((dig:name=Attribute), Atts).
parse_attribute(chain, _Atts, Elems, achain(Chain)) :- %SPEC
	parse_chain(Elems, Chain).

%parse_chain(+Elems, -Chain)
parse_chain([element(dig:attribute, Atts, _Elems)], [arole(Attribute)]) :- %SPEC arole kell?
	memberchk((dig:name=Attribute), Atts).
parse_chain([element(dig:feature, Atts, _Elems), Next| Elems], [arole(Feature)| Chain]) :-
	memberchk((dig:name=Feature), Atts), 
	parse_chain([Next|Elems], Chain).


%parse_concept(+ConceptType, +Atts, +Elems, -Concept)
parse_concept(top, _Atts, _Elems, top).
parse_concept(bottom, _Atts, _Elems, bottom).
parse_concept(catom, Atts, _Elems, aconcept(Concept)) :- 
	memberchk((dig:name=Concept), Atts). 

parse_concept(and, _Atts, Elems, and(Concepts)) :- 
	parse_concepts(Elems, Concepts).
parse_concept(or, _Atts, Elems, or(Concepts)) :- 
	parse_concepts(Elems, Concepts).
parse_concept(not, _Atts, 
			[element(dig:ConceptType, Atts, Elems)], Concept) :- 
	(
		ConceptType = not -> 
		Elems = [element(dig:ConceptType1, Atts1, Elems1)], %not(not()) eliminálása
		parse_concept(ConceptType1, Atts1, Elems1, Concept)
	;
		Concept = not(Concept1),
		parse_concept(ConceptType, Atts, Elems, Concept1)
	).

parse_concept(some, _Atts, 
			[element(dig:RoleType, AttsR, ElemsR), element(dig:ConceptType, AttsC, ElemsC)], 
			some(Role, Concept)) :- 
	parse_role(RoleType, AttsR, ElemsR, Role),
	parse_concept(ConceptType, AttsC, ElemsC, Concept).
parse_concept(all, _Atts, 
			[element(dig:RoleType, AttsR, ElemsR), element(dig:ConceptType, AttsC, ElemsC)], 
			all(Role, Concept)) :- 
	parse_role(RoleType, AttsR, ElemsR, Role),
	parse_concept(ConceptType, AttsC, ElemsC, Concept).

parse_concept(atmost, Atts, 
			[element(dig:RoleType, AttsR, ElemsR), element(dig:ConceptType, AttsC, ElemsC)], 
			atmost(N, Role, Concept)) :- 
	parse_role(RoleType, AttsR, ElemsR, Role), 
	parse_concept(ConceptType, AttsC, ElemsC, Concept),
	memberchk((dig:num=NA),Atts),
	atom_codes(NA,NC), number_codes(N,NC).
parse_concept(atleast, Atts,
			[element(dig:RoleType, AttsR, ElemsR), element(dig:ConceptType, AttsC, ElemsC)], 
			atleast(N, Role, Concept)) :- 
	parse_role(RoleType, AttsR, ElemsR, Role),
	parse_concept(ConceptType, AttsC, ElemsC, Concept),
	memberchk((dig:num=NA),Atts),
	atom_codes(NA,NC), number_codes(N,NC).

parse_concept(iset, Atts, Elems, or(Individuals)) :- %SPEC
	parse_individuals(Elems, Individuals),
	throw_concept_error(element(dig:iset, Atts, Elems)).


%concrete domain %SPEC
parse_concept(defined, Atts, [element(dig:AttributeType, AttsA, Elems)], some(Attribute, top)) :-  %TODO ? defined(Attribute)
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	throw_concept_error(element(dig:defined, Atts, [element(dig:AttributeType, Atts, Elems)])).
parse_concept(stringmin, Atts, [element(dig:AttributeType, AttsA, Elems)], stringmin(Val, Attribute)) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:val=Val),Atts), 
	throw_concept_error(element(stringmin, Atts, [element(dig:AttributeType, AttsA, Elems)])).
parse_concept(stringmax, Atts, [element(dig:AttributeType, AttsA, Elems)], stringmax(Val, Attribute)) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:val=Val),Atts), 
	throw_concept_error(element(dig:stringmax, Atts, [element(dig:AttributeType, AttsA, Elems)])).
% parse_concept(stringequals, Atts, [element(dig:AttributeType, AttsA, Elems)], stringequals(Val, Attribute)) :- %TODO így vagy úgy? 
	% parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	% memberchk((dig:val=Val),Atts), 
	% throw_concept_error(element(dig:stringequals, Atts, [element(dig:AttributeType, AttsA, Elems)])).
parse_concept(stringequals, Atts, [element(dig:AttributeType, AttsA, Elems)], 
		and([stringmin(Val, Attribute), stringmax(Val, Attribute)])) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:val=Val),Atts).
% parse_concept(stringrange, Atts, [element(dig:AttributeType, AttsA, Elems)], stringrange(Min, Max, Attribute)) :-
	% parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	% memberchk((dig:min=Min),Atts), 
	% memberchk((dig:max=Max),Atts), 
	% throw_concept_error(element(dig:stringrange, Atts, [element(dig:AttributeType, AttsA, Elems)])).
parse_concept(stringrange, Atts, [element(dig:AttributeType, AttsA, Elems)], 
		and([stringmin(Min, Attribute), stringmax(Max, Attribute)])) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:min=Min),Atts), 
	memberchk((dig:max=Max),Atts).
parse_concept(intmin, Atts, [element(dig:AttributeType, AttsA, Elems)], intmin(Val, Attribute)) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:min=Val),Atts), 
	throw_concept_error(element(dig:intmin, Atts, [element(dig:AttributeType, AttsA, Elems)])).
parse_concept(intmax, Atts, [element(dig:AttributeType, AttsA, Elems)], intmax(Val, Attribute)) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:max=Val),Atts),  
	throw_concept_error(element(dig:intmax, Atts, [element(dig:AttributeType, AttsA, Elems)])).
% parse_concept(intequals, Atts, [element(dig:AttributeType, AttsA, Elems)], intequals(Val, Attribute)) :-
	% parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	% memberchk((dig:val=Val),Atts),  
	% throw_concept_error(element(dig:intequals, Atts, [element(dig:AttributeType, AttsA, Elems)])).
parse_concept(intequals, Atts, [element(dig:AttributeType, AttsA, Elems)], 
		and([intmin(Val, Attribute), intmax(Val, Attribute)])) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:val=Val),Atts),  
	throw_concept_error(element(dig:intequals, Atts, [element(dig:AttributeType, AttsA, Elems)])).
% parse_concept(intrange, Atts, [element(dig:AttributeType, AttsA, Elems)], intrange(Min, Max, Attribute)) :-
	% parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	% memberchk((dig:min=Min),Atts), 
	% memberchk((dig:max=Max),Atts),  
	% throw_concept_error(element(dig:intrange, Atts, [element(dig:AttributeType, AttsA, Elems)])).
parse_concept(intrange, Atts, [element(dig:AttributeType, AttsA, Elems)], 
		and([intmin(Min, Attribute), intmax(Max, Attribute)])) :-
	parse_attribute(AttributeType, AttsA, Elems, Attribute), 
	memberchk((dig:min=Min),Atts), 
	memberchk((dig:max=Max),Atts), 
	throw_concept_error(element(dig:intrange, Atts, [element(dig:AttributeType, AttsA, Elems)])).

%parse_concepts(+Elems, -Concepts)
parse_concepts([element(dig:ConceptType, Atts, Elems)|ElemL], [Concept|Concepts]) :-
	parse_concept(ConceptType, Atts, Elems, Concept),
	parse_concepts(ElemL, Concepts).
parse_concepts([], []).

throw_concept_error(Elem) :- 
	get_dlog_option(dig_reader_fault_tolerance, yes) -> true
	; get_dlog_option(dig_reader_fault_tolerance, drop) -> fail
	;	throw(digerror(103, 'Unsupported Operation', Elem)).

%parse_individuals(+Elems, -Individuals)
parse_individuals([element(dig:individual, Atts, _Elems)|Elems], [iconcept(Individual)|Individuals]) :-
	memberchk((dig:name=Individual), Atts),
	parse_individuals(Elems, Individuals).
parse_individuals([], []).


