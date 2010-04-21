:- module(transitive, [collectInverses/2, addTransitiveInverses/3, removeTransitive/4]).

:- use_module(library(lists)).
:- use_module('nnf', [negNormForm/2]).
:- use_module('../struct').

% collectInverses(+Axioms,-Ibox)
% Axioms SHIQ axiomak listaja, melyek tartalmazhatnak inverz szerepeket
% Ibox olyan axiomak listaja, melyek ezen szerepek inverz voltat jelentik ki
collectInverses(Axioms,Inverses):-
	findall(I, (
		     contains_struct_substitute(Axioms,inv(arole(R))),
		     I = inv(inv(arole(R)), arole(R))
		   ), Is
	       ),
	sort(Is,Inverses).
	    
/***************************************************/
/********* Tranzitivitas kikuszobolese *************/
/***************************************************/	    

% addTransitiveInverses(+Trbox,+Ibox,Trbox2)
% ha egy szereprol tudjuk, hogy tranzitiv, es ismerjuk az inverzet,
% akkor az inverzrol is kijelentjuk, hogy tranzitiv
addTransitiveInverses([],_,[]).
addTransitiveInverses([R|Trbox],Ibox,[R,S|Trbox2]):-
	(	member(inv(R,S),Ibox)
	;	member(inv(S,R),Ibox)
	), !,	
	addTransitiveInverses(Trbox,Ibox,Trbox2).
addTransitiveInverses([R|Trbox],Ibox,[R|Trbox2]):-	
	addTransitiveInverses(Trbox,Ibox,Trbox2).
	


% removeTransitive(+Tbox,+Hbox,+Trbox,-TransTbox)
% TransTbox tartalmazza azon axiomaknak megfelelo bennsositett fogalmakat
% melyeket hozzaadva Tbox-ban levo bennsositett fogalmakhoz
% a Trbox-ban levo tranzitiv axiomak elhagyhatoak
removeTransitive(_,_,[],[]):- !.
removeTransitive(Tbox,Hbox,Trbox,TransTbox):-
	getClosure(Tbox,Closure),
	getNewAxioms(Closure,Hbox,Trbox,TransTbox).

% getClosure(+Tbox,-Closure)
%	Closure tartalmazza azon fogalmakat, melyekre
%	a tranzitiv axiomak hatassal lehetnek
getClosure([],[]).
getClosure([X|Tbox],Closure):-
	universalSubConcepts(X,SubCon),
	append(SubCon,Rest,Closure),
	getClosure(Tbox,Rest).
	
% universalSubConcepts(+Concept,-SubCon)
%	SubCon azon all(R,C) kifejezesek listaja,
%	melyek elofordulnak reszfogalomkent Concept-ben
universalSubConcepts(and(Cs),SubCon):- !,
	universalSubConceptsList(Cs,SubCon).
universalSubConcepts(or(Cs),SubCon):-	!,
	universalSubConceptsList(Cs,SubCon).	
universalSubConcepts(atmost(0,R,C),[atmost(0,R,C)|Rest]):- !,
	negNormForm(not(C),NC),
	universalSubConcepts(NC,Rest).
universalSubConcepts(atmost(_,_,C),SubCon):-	!,
	negNormForm(not(C),X),
	universalSubConcepts(X,SubCon1),
	universalSubConcepts(C,SubCon2),
	append(SubCon1,SubCon2,SubCon).		
universalSubConcepts(atleast(_,_,C),SubCon):- !,
	universalSubConcepts(C,SubCon).	
universalSubConcepts(_,[]).


% universalSubConceptsList(+Cs,-SubCon):-
%	SubCon azon all(arole(R),C) kifejezesek listaja,
%	melyek elofordulnak reszfogalomkent Cs valamelyik elemeben
universalSubConceptsList([],[]).
universalSubConceptsList([C|Cs],SubCon):-
	universalSubConcepts(C,SubCon1),
	append(SubCon1,Rest,SubCon),
	universalSubConceptsList(Cs,Rest).
	
% getNewAxioms(+Closure,+Hbox,+Trbox,-NewAxioms)
% Closure all(R,C) alaku fogalmak listaja
% Trbox tartalmazza a tranzitiv axiomakat
% Hbox tartalmazza a szerephierarchiat
% NewAxioms azon all(R,C) -> all(S,all(S,C)) axiomak listaja, melyre
% Itt mar nincsen some/2 illetve all/2, csak atleast(1,_,_) illetve atmost(0,_,_)
%	S reszszerepe R-nek es S tranzitiv
getNewAxioms([atmost(0,R,C)|Closure],Hbox,Trbox,NewAxioms):-
	findall(S,(
		   member(S,Trbox),			
		   someSubRole(S,R,Hbox)
		  ),Ss),
	newAxiomList(R,C,Ss,NewAxioms1),
	append(NewAxioms1,Rest,NewAxioms),
	getNewAxioms(Closure,Hbox,Trbox,Rest).
getNewAxioms([],_,_,[]).


% newAxiomList(+R,+C,+Ss,-NewAxioms)
newAxiomList(_,_,[],[]).
newAxiomList(R,C,[S|Ss], [or([atleast(1,R,C),atmost(0,S,atleast(1,S,C))])|NewAxioms]):-
	newAxiomList(R,C,Ss,NewAxioms).
	
% someSubRole(+S,+R,+Hbox)
% adott Hbox szerephierarchia mellett
% S reszszerepe R-nek
someSubRole(S,S,_):- !.
someSubRole(S,R,Hbox):-
	member(subrole(S,R),Hbox), !.
someSubRole(S,R,Hbox):-
	member(subrole(S,P),Hbox),
	someSubRole(P,R,Hbox), !.