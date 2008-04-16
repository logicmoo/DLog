:- module(query, [query/4]).

%query(Query, TBox, ABox, Answer).
query(allConceptNames, _TBox, _ABox, conceptSet(_Answer)). %TODO feldolgozás során lementeni? vagy itt összeszedni?
			%[[Concept, ...], ...]
query(allRoleNames, _TBox, _ABox, roleSet(_Answer)). %[[Role, ...], ...]
query(allIndividuals, _TBox, _ABox, individualSet(_Answer)).

query(instances(ConceptTerm), TBox, _ABox, individualSet(Answer)) :-
	ConceptTerm = aconcept(Concept),
	current_predicate(TBox:Concept/1) 
	->
	(setof(
		I,
		call(TBox:Concept, I), %TODO: összetett concept?
		Answer) -> true 
	; Answer = [] %üres válasz? --> []
	).
query(instance(Name, ConceptTerm), TBox, _ABox, Answer) :-
	ConceptTerm = aconcept(Concept),
	current_predicate(TBox:Concept/1) 
	->
	(
	call(TBox:Concept, Name) -> !, Answer = true
		; Answer = false
	).
query(roleFillers(Name, RoleTerm), TBox, _ABox, individualSet(Answer)) :-
	RoleTerm = arole(Role),
	current_predicate(TBox:Role/2) 
	->
	(setof(
		I,
		call(TBox:Role, Name, I), %TODO: inv?, arole(Role)?
		Answer) -> true 
	; Answer = []).
query(relatedIndividuals(RoleTerm), TBox, _ABox, individualPairSet(Answer)) :-
	RoleTerm = arole(Role),
	current_predicate(TBox:Role/2) 
	->
	(setof(
		I1-I2,
		call(TBox:Role, I1, I2), %TODO: inv?, arole(Role)?
		Answer) -> true 
	; Answer = []).
