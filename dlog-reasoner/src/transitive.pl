:- module(transitive, [collectInverses/2, separateBoxes/4, addTransitiveInverses/3, removeTransitive/4]).

:- use_module(library(lists)).
:- use_module(dl_to_fol).
:- use_module(struct).
:- use_module(show).

/***************************************************/
/********* KB szetbontasa **************************/
/***************************************************/

% collectInverses(+Axioms,-Ibox)
% Axioms SHIQ axiomak listaja, melyek tartalmazhatnak inverz szerepeket
% Ibox olyan axiomak listaja, melyek ezen szerepek inverz voltat jelentik ki
collectInverses(Axioms,Inverses):-
	findall(I, (
		     contains_struct2(Axioms,arole(R)),
		     atom_codes(R,NameList),
		     append(Prefix,Suffix,NameList),
		     atom_codes('inv_',Prefix),
		     atom_codes(InvRole,Suffix),
		     I = inv(arole(InvRole), arole(R))
		   ), Is
	       ),
	sort(Is,Inverses).
	    

%	separateBoxes(+Axioms,-Tbox,-Hbox,-Transbox)
% Tbox terminologiai, Hbox hierarchias es Transbox tranzitivas
% axiomak unioja Axioms
separateBoxes([],[],[],[]).

separateBoxes([implies(C1, C2)|Ax],[implies(C1, C2)|T],H,Tr):-
	separateBoxes(Ax,T,H,Tr).

separateBoxes([equiv(C1, C2)|Ax],[implies(C1, C2),implies(C2,C1)|T],H,Tr):-
	separateBoxes(Ax,T,H,Tr).

separateBoxes([trans(R)|Ax],T,H,[R|Tr]):-
	separateBoxes(Ax,T,H,Tr).

separateBoxes([subrole(R1,R2)|Ax],T,[subrole(R1,R2)|H],Tr):-
	separateBoxes(Ax,T,H,Tr).


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
% TransTbox tartalmazza azon axiomakat, melyeket hozzaadva Tbox-hoz
% a Trbox-ban levo tranzitiv axiomak elhagyhatoak
removeTransitive(_,_,[],[]):- !.

removeTransitive(Tbox,Hbox,Trbox,TransTbox):-
	getClosure(Tbox,Closure),
	getNewAxioms(Closure,Hbox,Trbox,TransTbox).

% getClosure(+Tbox,-Closure)
%	Closure tartalmazza azon fogalmakat, melyekre
%	a tranzitiv axiomak hatassal lehetnek
getClosure([],[]).
getClosure([implies(C1,C2)|Tbox],Closure):-
	negNormForm(or([not(C1),C2]),X),
	universalSubConcepts(X,SubCon),
	append(SubCon,Rest,Closure),
	getClosure(Tbox,Rest).
	
% universalSubConcepts(+Concept,-SubCon)
%	SubCon azon all(arole(R),C) kifejezesek listaja,
%	melyek elofordulnak reszfogalomkent Concept-ben
universalSubConcepts(and(Cs),SubCon):- !,
	universalSubConceptsList(Cs,SubCon).
universalSubConcepts(or(Cs),SubCon):-	!,
	universalSubConceptsList(Cs,SubCon).	
universalSubConcepts(all(arole(R),C),[all(arole(R),C)|Rest]):- !,
	universalSubConcepts(C,Rest).
universalSubConcepts(some(arole(_),C),SubCon):-	!,
	universalSubConcepts(C,SubCon).	
universalSubConcepts(atmost(_,arole(_),C),SubCon):-	!,
	negNormForm(not(C),X),
	universalSubConcepts(X,SubCon1),
	universalSubConcepts(C,SubCon2),
	append(SubCon1,SubCon2,SubCon).		
universalSubConcepts(atleast(_,arole(_),C),SubCon):- !,
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
% Closure all(arole(R),C) alaku fogalmak listaja
% Trbox tartalmazza a tranzitiv axiomakat
% Hbox tartalmazza a szerephierarchiat
% NewAxioms azon all(S,C) -> all(S,all(S,C)) axiomak listaja, melyre
%	S reszszerepe R-nek es S tranzitiv
getNewAxioms([all(arole(R),C)|Closure],Hbox,Trbox,NewAxioms):-
	findall(S,(
		   member(S,Trbox),			
		   someSubRole(S,arole(R),Hbox)			
		  ),Ss),			
	newAxiomList(arole(R),C,Ss,NewAxioms1),
	append(NewAxioms1,Rest,NewAxioms),
	getNewAxioms(Closure,Hbox,Trbox,Rest).

getNewAxioms([],_,_,[]).


% newAxiomList(+R,+C,+Ss,-NewAxioms)
newAxiomList(_,_,[],[]).
newAxiomList(R,C,[S|Ss], [implies(all(R,C),all(S,all(S,C)))|NewAxioms]):-
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