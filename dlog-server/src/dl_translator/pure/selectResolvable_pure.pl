:- module(selectResolvable_pure, [selectResolvableList/2, selectResolvable/2, greater/2]).

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
	selectResolvable(Greatest1,Greatest).
selectResolvable(atmost(N,R,C),atmost(N,R,C2)):- !,
	selectResolvable(C,C2).
selectResolvable(atleast(N,R,C),atleast(N,R,C2)):- !,
	selectResolvable(C,C2).
selectResolvable(C,C).


% getGreatest(+List,-Greatest, Rest)
% List fogalmak listaja listaja, melynek
% legnagyobb eleme Greatest, Rest pedig a tobbi
getGreatest(List,Greatest,Rest):-
	getGreatest(List,[],Greatest,Rest).

% getGreatest(+List,+Smaller,-Greatest, -Rest)
% List fogalmak listaja listaja, melynek
% legnagyobb eleme Greatest, Rest pedig a tobbihez hozzafuzve
% Smaller elemeit
getGreatest([D],Rest,D,Rest):- !.
getGreatest([D1,D2|Ds], Smaller, Greatest, Rest):-
	( greater(D1,D2) -> getGreatest([D1|Ds],[D2|Smaller],Greatest,Rest)
	; getGreatest([D2|Ds],[D1|Smaller],Greatest,Rest)
	).

% greater(+A,+B): A es B fogalmak es A>B
greater(atleast(_,_,_),_):- !.
greater(_,atleast(_,_,_)):- !, fail.
greater(atmost(N1,R1,C1),atmost(N2,R2,C2)):- !,
	(
	  greater_role(R1,R2) -> true
	; R1 = R2, N1 > N2 -> true
	; R1 = R2, N1 = N2, greater(C1,C2) -> true
	; fail
	).
greater(atmost(_,_,_),_):- !.
greater(_,atmost(_,_,_)):- !, fail.

greater(or(Cs),or(Ds)):- !,
	getGreatest(Cs,C,CRest),
	getGreatest(Ds,D,DRest),
	(
	  greater(C,D) -> true
	; C = D ->
	  (
	    CRest = [] -> fail
	  ; DRest = [] -> true
	  ; greater(or(CRest),or(DRest))
	  )
	).
greater(or(Cs),D):- !,
	getGreatest(Cs,C,_),
	(
	  greater(C,D) -> true
	; C = D -> true
	).
greater(C,or(Ds)):- !,
	getGreatest(Ds,D,_),
	greater(C,D).

greater(and(Cs),and(Ds)):- !,
	getGreatest(Cs,C,CRest),
	getGreatest(Ds,D,DRest),
	(
	  greater(C,D) -> true
	; C = D ->
	  (
	    CRest = [] -> fail
	  ; DRest = [] -> true
	  ; greater(and(CRest),and(DRest))
	  )
	).
greater(and(Cs),D):- !,
	getGreatest(Cs,C,_),
	(
	  greater(C,D) -> true
	; C = D -> true
	).
greater(C,and(Ds)):- !,
	getGreatest(Ds,D,_),
	greater(C,D).

greater(not(C),C):- !.
greater(not(C),D):- !,
	greater(C,D).
greater(C,not(D)):- !,
	greater(C,D).
	
greater(aconcept(C),aconcept(D)):- !,
	C @> D.
	  

greater_role(arole(R),arole(S)):- !,
	R @> S.
greater_role(inv(R),R):- !.
greater_role(inv(R),S):- !,
	greater_role(R,S).
greater_role(R,inv(S)):- !,
	greater_role(R,S).