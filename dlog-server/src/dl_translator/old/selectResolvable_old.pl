:- module(selectResolvable_old, [selectResolvable/2, greater/2, cls_to_ocls/2, getGreatest/2,select_precisely/3]).

:- use_module('../struct').
:- use_module(library(lists), [select/3, append/3]).

% selectResolvable(+Cs,-OCs)
%	Cs klozhalmazban a rezolvalhato literalok
%	elolre hozasaval kapjuk OCs klozhalamzt
selectResolvable([],[]).
selectResolvable([C|Cs],[OC|OCs]):-
	cls_to_ocls(C,OC),
	selectResolvable(Cs,OCs).

% cls_to_ocls(+Cls,-OCls): Cls klozban a rezolvalhato
% literal(ok) elorehozasaval kapjuk OCls-t
cls_to_ocls([T,[]],[T,[]]) :- !.
cls_to_ocls([T,[Cls]],[T,[Cls]]):- !.
cls_to_ocls([1,Cls],[1,[Selected|Rest]]):- !,
	select(Selected,Cls,Rest),
	Selected = not(arole(_,_,_)), !.
cls_to_ocls([3,Cls],[3,[Selected|Rest]]):- !,
	select(Selected,Cls,Rest),
	Selected = arole(_,_,_), !.
cls_to_ocls([30,Cls],[30,[Selected|Rest]]):- !,
	select(Selected,Cls,Rest),
	Selected = arole(_,_,_), !.
cls_to_ocls([4,Cls],[4,[Selected|Rest]]):- !,
	select(Selected,Cls,Rest),
	Selected = arole(_,_,_), !.
cls_to_ocls([7,Cls],[7-N,Ordered]):- !,
	selectNegBin(Cls,NegBin,Rest),
	append(NegBin,Rest,Ordered),
	length(NegBin,N).

cls_to_ocls([50,Cls],[50,[Selected|Rest]]):-
	contains_struct(Cls,fun(_,_)), !,
	selectNegBin(Cls,NegBin,Others),
	getGreatest(Others,Selected),
	select_precisely(Selected,Others,OthersRest),
	append(NegBin,OthersRest,Rest).
cls_to_ocls([50,Cls],[50-N,Ordered]):-
	contains_struct(Cls,arole(_,_,_)), !,
	selectNegBin(Cls,NegBin,Rest),
	append(NegBin,Rest,Ordered),
	length(NegBin,N).

cls_to_ocls([T,Cls],[T,[Selected|Rest]]):- !,
	getGreatest(Cls,Selected),
	select_precisely(Selected,Cls,Rest), !.

% selectNegBin(+Cls,-Bin,-Rest): Cls literallistabol kivalogatva a negalt binarisokat
% kapjuk Bin-t, a maradek pedig Rest
selectNegBin([],[],[]).
selectNegBin([not(arole(A,B,C))|Cls],[not(arole(A,B,C))|Bin],Rest):- !,
	selectNegBin(Cls,Bin,Rest).
selectNegBin([C|Cls],Bin,[C|Rest]):- !,
	selectNegBin(Cls,Bin,Rest).


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
% List egyargumentumu literalok listaja, melynek
% legnagyobb eleme Greatest
% lehetnek egyenlosegek is
getGreatest([D],D).
getGreatest([D1,D2|Ds],Maximal):-
	( greater(D1,D2) -> getGreatest([D1|Ds],Maximal)
	; getGreatest([D2|Ds],Maximal)
	).
	
	
% greater(+A,+B): A es B egyargumentumu literal es A>B, felteve, hogy
% azonos valtozot tartalmaznak
% lehet egyenloseg is
greater(A,B):-
	greatness(A,GA),
	greatness(B,GB),
	GA @> GB, !.

	
% greatness(+L,-G)
% L egy literal vagy term, melyhez hozzarendelunk egy G
% atomot, hogy tudjuk a literalokat illetve termeket rendezni
% ketargumentumu predikatumok kozul csak az egyenloseget kezeli
greatness(X,'0'):- % X
	var(X), !.
greatness(fun(F,X),G):- % f(X)
	var(X), !,
	atom_concat('1',F,G).
greatness(fun(F1,fun(F2,_)),G):- % f(g(X))
	atom_concat('2',F1,G1),
	atom_concat(G1,F2,G).
greatness(not(X),G):- % not(X)
	greatness(X,GX),
	atom_concat(GX,'1',G).
greatness(eq(X,_),0):-
	var(X), !.
greatness(eq(X,Y),G):-
	greatness(X,GX),
	greatness(Y,GY),
	atom_concat(GX,'3',G1),
	atom_concat(G1,GY,G).
greatness(aconcept(C,X),G):-
	greatness(X,GX),
	atom_concat(GX,'2',G1),
	atom_concat(G1,C,G).
greatness(nconcept(C,X),G):-
	greatness(X,GX),
	atom_concat(GX,'1',G1),
	atom_concat(G1,C,G).
	
	
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

