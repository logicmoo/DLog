:- module(selectResolvable, [selectResolvableList/2,selectResolvable/2,greater_term/2]).

:- use_module('../struct').
:- use_module(library(lists)).

% selectResolvable(+Cs,-OCs)
% Cs klozhalmazban a rezolvalhato literalok
% elolre hozasaval kapjuk OCs klozhalamzt
selectResolvableList([],[]).
selectResolvableList([C|Cs],[OC|OCs]):-
	selectResolvable(C,OC),
	selectResolvableList(Cs,OCs).


selectResolvable(Cls,[Selected|Rest]):-
	getGreatest(Cls,Selected),
	select_precisely(Selected,Cls,Rest).

% select_precisely(+Element,+List,+Rest)
% ugyanaz, mint a konyvtari select, csak
% nincs egyesites
select_precisely(A,L,R):-
	select_precisely(A,L,[],R).
select_precisely(_,[],R,R).
select_precisely(A,[L|Ls],T,R):-
	A == L, !,
	append(Ls,T,R).
select_precisely(A,[L|Ls],T,R):-
	select_precisely(A,Ls,[L|T],R).
	
	
	
% getGreatest(+List,-Greatest)
% List literalok listaja, melynek
% legnagyobb eleme Greatest
getGreatest([D],D).
getGreatest([D1,D2|Ds],Maximal):-
	( greater_literal(D1,D2) -> getGreatest([D1|Ds],Maximal)
	; getGreatest([D2|Ds],Maximal)
	).

greater_term(A,B):-
	greatness_term(A,GA),
	greatness_term(B,GB),
	GA @> GB, !.
	
	
% greater_literal(+A,+B): A es B literalok es A>B, felteve, hogy
% azonos valtozot tartalmaznak
greater_literal(A,B):-
	greatness_literal(A,GA),
	greatness_literal(B,GB),
	GA @> GB, !.

% greatness_term(+T,-G)
% T egy term, melyhez hozzarendelunk egy G
% atomot, hogy tudjuk a termeket rendezni
greatness_term(X,'0'):-
	var(X), !.
greatness_term(fun(F,X,_),G):- 	% f(X)
	var(X), !,
	atom_concat('1',F,G).
greatness_term(fun(F1,fun(F2,_,_),_),G):- !, % f(g(X))
	atom_concat('2',F2,G1),
	atom_concat(G1,F1,G).



% greatness_conceptname(+C,-G)
% C egy osztalynev, melyhez hozzarendelunk egy G
% atomot, hogy tudjuk a fogalomneveket rendezni
greatness_conceptname(aconcept(C),G):-
	atom_concat('2',C,G).
greatness_conceptname(nconcept(C),G):-
	atom_concat('1',C,G).


	
% greatness_literal(+L,-G)
% L egy literal, melyhez hozzarendelunk egy G
% atomot, hogy tudjuk a literalokat rendezni
greatness_literal(not(L),G):- !,
	greatness_literal(L,GL),
	atom_concat(GL,'0',G).
greatness_literal(role(eq,X,Y),G):- !,
	greatness_term(X,GX),
	greatness_term(Y,GY),
	atom_concat(GX,GY,G1),
	atom_concat(G1,'2',G).
greatness_literal(role(_,X,Y),G):- !,
	greatness_term(X,GX),
	greatness_term(Y,GY),
	atom_concat(GX,GY,G1),
	atom_concat(G1,'3',G).
greatness_literal(concept(C,X),G):- !,
	greatness_term(X,GX),
	greatness_conceptname(C,GC),
	atom_concat(GX,'0',G1),
	atom_concat(G1,GC,G2),
	atom_concat(G2,'1',G).
greatness_literal(_,'0').


% not used, only for testing
testOrdering([]).
testOrdering([L|Ls]):-
	greatness_literal(L,G),
	nl, print(L), print('\t\t'), print(G),
	testOrdering(Ls).
