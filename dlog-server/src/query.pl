:- module(query, [query/4]).

%query(Query, TBox, ABox, Answer).
query(allConceptNames, _TBox, _ABox, conceptSet(_Answer)). %TODO feldolgozás során lementeni? vagy itt összeszedni?
			%[[Concept, ...], ...]
query(allRoleNames, _TBox, _ABox, roleSet(_Answer)). %[[Role, ...], ...]
query(allIndividuals, _TBox, _ABox, individualSet(_Answer)).

query(instances(Concept), TBox, _ABox, individualSet(Answer)) :-
	catch(
		(setof(
			I,
			call(TBox:Concept, I), %TODO: összetett concept?
			Answer		
		) -> true ; Answer = []), %üres válasz? --> []
		error(existence_error(procedure, TBox:Concept/1), _),
		fail %nem létezö concept --> fail
	).
query(instance(Name, Concept), TBox, _ABox, Answer) :-
	catch(
		(call(TBox:Concept, Name) -> !, Answer = true
		; Answer = false),
		error(existence_error(procedure, TBox:Concept/1), _),
		fail
	).
query(roleFillers(Name, Role), TBox, _ABox, individualSet(Answer)) :-
	catch(
		(setof(
			I,
			call(TBox:Role, Name, I), %TODO: inv?, arole(Role)?
			Answer		
		) -> true ; Answer = []),
		error(existence_error(procedure, TBox:Role/2), _),
		fail
	).
query(relatedIndividuals(Role), TBox, _ABox, individualPairSet(Answer)) :-
	catch(
		(setof(
			I1-I2,
			call(TBox:Role, I1, I2), %TODO: inv?, arole(Role)?
			Answer
		) -> true ; Answer = []),
		error(existence_error(procedure, TBox:Role/2), _),
		fail
	).

