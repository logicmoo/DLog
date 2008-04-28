:- module(transitive, [removeTransitive/4, inv/2]).

:- use_module(library(lists)).
:- use_module('dl_to_fol', [negNormForm/2]).

% removeTransitive(+NNF,+RInclusion,+Transitive,-TransNNF)
% TransNNF tartalmazza azon fogalomtartalmazasi axiomak belsositett
% es normalizalt alakjat, melyeket
% hozzaadva NNF szinten belsositett es normalizalt axiomakhoz
% a Transitive listaban levo szerepek tranzitivitasa elhagyhato
removeTransitive(_,_,[],[]):- !.

removeTransitive(NNF,RInclusion,Transitive,TransNNF):-
	getClosure(NNF,Closure),
	getNewAxioms(Closure,RInclusion,Transitive,TransNNF).

% getClosure(+NNF,-Closure)
% Closure tartalmazza azon fogalmakat, melyekre
% a tranzitiv axiomak hatassal lehetnek
getClosure([],[]).
getClosure([C|NNF],Closure):-
	universalSubConcepts(C,SubCon),
	append(SubCon,Rest,Closure),
	getClosure(NNF,Rest).
	
% universalSubConcepts(+Concept,-SubCon)
% SubCon azon atmost(0,R,C) kifejezesek listaja,
% melyek elofordulnak reszfogalomkent Concept-ben
universalSubConcepts(and(Cs),SubCon):- !,
	universalSubConceptsList(Cs,SubCon).
universalSubConcepts(or(Cs),SubCon):-	!,
	universalSubConceptsList(Cs,SubCon).	
universalSubConcepts(atmost(0,R,C),[atmost(0,R,C)|Rest]):- !,
	universalSubConcepts(C,Rest).
universalSubConcepts(atmost(_,arole(_),C),SubCon):-	!,
	negNormForm(not(C),X),
	universalSubConcepts(X,SubCon1),
	universalSubConcepts(C,SubCon2),
	append(SubCon1,SubCon2,SubCon).		
universalSubConcepts(atleast(_,_,C,_),SubCon):- !,
	universalSubConcepts(C,SubCon).	
universalSubConcepts(_,[]).


% universalSubConceptsList(+Cs,-SubCon):-
% SubCon azon all(arole(R),C) kifejezesek listaja,
% melyek elofordulnak reszfogalomkent Cs valamelyik elemeben
universalSubConceptsList([],[]).
universalSubConceptsList([C|Cs],SubCon):-
	universalSubConcepts(C,SubCon1),
	append(SubCon1,Rest,SubCon),
	universalSubConceptsList(Cs,Rest).
	
% getNewAxioms(+Closure,+RInclusion,+Transitive,-NewAxioms)
% Closure all(arole(R),C) alaku fogalmak listaja
% Transitive tartalmazza a tranzitiv szerepeket
% RInclusion tartalmazza a szerephierarchiat
% NewAxioms azon all(R,C) -> all(S,all(S,C)) axiomak listaja, melyre
% S reszszerepe R-nek es S tranzitiv (belsositett es normalizalt
% alakban)
getNewAxioms([atmost(0,R,C)|Closure],RInclusion,Transitive,NewAxioms):-
	findall(S,(
		   (
		     member(S,Transitive)
		   ; inv(S,Sinv),
		     member(Sinv,Transitive)
		   ),
		   someSubRole(S,R,RInclusion)
		  ),Ss
	       ),
	newAxiomList(R,C,Ss,NewAxioms1),
	append(NewAxioms1,Rest,NewAxioms),
	getNewAxioms(Closure,RInclusion,Transitive,Rest).

getNewAxioms([],_,_,[]).


% someSubRole(+S,+R,+RInclusion)
% adott Hbox szerephierarchia mellett
% S reszszerepe R-nek
someSubRole(S,S,_):- !.
someSubRole(S,R,RInclusion):-
	member(subrole(S,R),RInclusion), !.
someSubRole(S,R,RInclusion):-
	member(subrole(S,P),RInclusion),
	someSubRole(P,R,RInclusion), !.

% newAxiomList(+R,+C,+Ss,-NewAxioms)
newAxiomList(_,_,[],[]).
newAxiomList(R,C,[S|Ss],[A|NewAxioms]):-
	negNormForm(or([atleast(1,R,C,[C]),atmost(0,S,atleast(1,S,C,[C]))]),A),
	newAxiomList(R,C,Ss,NewAxioms).


% inv(S,R): - S szerep inverze R
inv(arole(R),inv(arole(R))).
inv(inv(arole(R)),arole(R)).

