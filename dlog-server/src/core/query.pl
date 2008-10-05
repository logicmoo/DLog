:- module(query, [query/4]).

:- use_module(config, [target/1]).
:- use_module('../prolog_translator/predicate_names', [predicate_name/2]).
:- target(sicstus) -> use_module(core_sicstus_tools, [call/2, call/3])
	; true.

%query(Query, TBox, ABox, Answer).
query(allConceptNames, _TBox, _ABox, conceptSet(_Answer)). %TODO feldolgozás során lementeni? vagy itt összeszedni?
			%[[Concept, ...], ...]
query(allRoleNames, _TBox, _ABox, roleSet(_Answer)). %[[Role, ...], ...]
query(allIndividuals, _TBox, _ABox, individualSet(_Answer)).

query(instances(ConceptTerm), TBox, _ABox, individualSet(Answer)) :-
	(	ConceptTerm = aconcept(Concept) %TODO: összetett concept?
	-> 	predicate_name(Concept, Name)
	;	ConceptTerm = not(aconcept(Concept)) 
	->	predicate_name(not(Concept), Name)
	),
	current_predicate(TBox:Name/1),
	findall(I, call(TBox:Name, I), Answer).
query(instance(Individual, ConceptTerm), TBox, _ABox, Answer) :-
	(	ConceptTerm = aconcept(Concept) 
	-> 	predicate_name(Concept, Name)
	;	ConceptTerm = not(aconcept(Concept)) 
	->	predicate_name(not(Concept), Name)
	),
	current_predicate(TBox:Name/1),
	(	call(TBox:Name, Individual)
	->	!, 
		Answer = true
	;	Answer = false
	).
query(roleFillers(Individual, RoleTerm), TBox, _ABox, individualSet(Answer)) :-
	RoleTerm = arole(Role), %TODO: inv?, arole(Role)?
	predicate_name(Role, Name),
	current_predicate(TBox:Name/2),
	findall(I, call(TBox:Name, Individual, I), Answer).
query(relatedIndividuals(RoleTerm), TBox, _ABox, individualPairSet(Answer)) :-
	RoleTerm = arole(Role), %TODO: inv?, arole(Role)?
	predicate_name(Role, Name),
	current_predicate(TBox:Name/2),
	findall(I1-I2, call(TBox:Name, I1, I2), Answer).
