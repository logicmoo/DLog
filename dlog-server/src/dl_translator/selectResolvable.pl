:- module(selectResolvable, [selectResolvableList/2, selectResolvable/2]).

:- use_module(struct).
:- use_module(library(lists), [select/3, append/3]).

% selectResolvableList(+Cs,-OCs)
% Cs fogalomhalmazban a rezolvalhato literalok
% elolre hozasaval kapjuk OCs fogalomhalamzt
selectResolvableList([],[]).
selectResolvableList([C|Cs],[OC|OCs]):-
	selectResolvable(C,OC),
	selectResolvableList(Cs,OCs).

% selectResolvable(+C,-OC): C fogalomban a rezolvalhato
% literal elorehozasaval kapjuk OC-t
selectResolvable(or(L),or([Greatest|Rest])):- !,
	getGreatest(L,Greatest1,Rest),
	(
	  Greatest1 = atmost(N,R,C) ->
	  selectResolvable(C,C2),
	  Greatest = atmost(N,R,C2)
	; Greatest1 = atleast(N,R,C,Sel) ->
	  selectResolvable(C,C2),
	  Greatest = atleast(N,R,C2,Sel)
	; Greatest = Greatest1
	).
selectResolvable(atmost(N,R,C),atmost(N,R,C2)):- !,
	selectResolvable(C,C2).
selectResolvable(atleast(N,R,C,Sel),atleast(N,R,C2,Sel)):- !,
	selectResolvable(C,C2).
selectResolvable(C,C).


% getGreatest(+List,-Greatest, Rest)
% List fogalmak listaja listaja, melynek
% legnagyobb eleme Greatest, Rest pedig a tobbi
getGreatest(List,Greatest,Rest):-
	getGreatest(List,[],Greatest,Rest).

% getGreatest(+List,+Smaller,-Greatest, Rest)
% List fogalmak listaja listaja, melynek
% legnagyobb eleme Greatest, Rest pedig a tobbihez hozzafuzve
% Smaller elemeit
getGreatest([D],Rest,D,Rest):- !.
getGreatest([D1,D2|Ds], Smaller, Greatest, Rest):-
	( greater(D1,D2) -> getGreatest([D1|Ds],[D2|Smaller],Greatest,Rest)
	; getGreatest([D2|Ds],[D1|Smaller],Greatest,Rest)
	).
	
	
% greater(+A,+B): A es B fogalmak es A>B
greater(A,B):-
	greatness(A,GA),
	greatness(B,GB),
	GA @> GB.

	
% greatness(+L,-G)
% L egy DL kifejezes, melyhez hozzarendelunk egy G
% atomot, hogy tudjuk a kifejezeseket rendezni
greatness(atleast(_,_,_,_),'4').
greatness(atmost(N,R,C),G):-
	greatness(R,GR),
	greatness(C,GC),
	atom_concat('3',GR,G1),
	atom_concat(G1,N,G2),
	atom_concat(G2,GC,G).
greatness(and(L),G):-
	getGreatest(L,Greatest,_),
	greatness(Greatest,G).
greatness(or(L),G):-
	getGreatest(L,Greatest,_),
	greatness(Greatest,G).
greatness(not(C),G):-
	greatness(C,G1),
	atom_concat(G1,'2',G).
greatness(aconcept(C),G):-
	atom_concat('1',C,G).
greatness(nconcept(C),G):-
	atom_concat('0',C,G).
greatness(arole(R),G):-
	atom_concat(R,'0',G).
greatness(inv(arole(R)),G):-
	atom_concat(R,'1',G).

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
	
	
	

	
/*************************** teszt **************************/
/*
test1:-
	greater(fun(f,X),X). % yes
test2:-
	greater(X,fun(f,X)). % no
test3:-
	greater(fun(f,fun(g,X)),fun(h,X)). % yes
test4:-
	greater(fun(f,fun(g,X)),fun(h,fun(k,X))). % no
test5:-
	greater(aconcept(okos,X), aconcept(szep,X)). % no
test6:-
	greater(aconcept(szep,X), aconcept(okos,X)). % yes
test7:-
	greater(aconcept(okos,fun(f,X)), aconcept(szep,X)). % yes
test8:-
	greater(aconcept(okos,fun(f,X)), aconcept(szep,fun(f,marked(fun(g,X))))). % no
*/

