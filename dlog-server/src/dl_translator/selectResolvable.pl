:- module(selectResolvable, [selectResolvableList/2, selectResolvable/2, greater/2]).

:- use_module(library(lists), [select/3, append/3]).

% selectResolvableList(+Cs,-OCs)
% Cs fogalomhalmazban a rezolvalhato literalok
% elolre hozasaval kapjuk OCs fogalomhalamzt
selectResolvableList([],[]).
selectResolvableList([C|Cs],[OC|OCs]):-
	selectResolvable(C,OC),
% 	arrange(C,OC),
	selectResolvableList(Cs,OCs).

% selectResolvable(+C,-OC): C fogalomban a rezolvalhato
% literal elorehozasaval kapjuk OC-t
selectResolvable(or(L),or([Greatest|Rest])):- !,
	getGreatest(L,Greatest1,Rest),
	(
	  Greatest1 = atmost(N,R,C,L) ->
	  selectResolvable(C,C2),
	  Greatest = atmost(N,R,C2,L)
	; Greatest1 = atleast(N,R,C,Sel) ->
	  selectResolvable(C,C2),
	  Greatest = atleast(N,R,C2,Sel)
	; Greatest = Greatest1
	).
selectResolvable(atmost(N,R,C,L),atmost(N,R,C2,L)):- !,
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
greatness(atleast(_,_,_,[_]),'50'):- !.
greatness(atleast(_,_,_,[_,_]),'51'):- !.
greatness(atleast(_,_,_,[_,_,_]),'52'):- !.

greatness(atmost(N,R,C,_),G):-
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
greatness(top,'0').
greatness(bottom,'0').
greatness(arole(R),G):-
	atom_concat(R,'0',G).
greatness(inv(arole(R)),G):-
	atom_concat(R,'1',G).



arrange(or(L),or(Arranged)):- !,
	arrangeList(L,Arranged).
arrange(and(L),and(Arranged)):- !,
	arrangeList(L,Arranged).
arrange(atmost(N,R,C,L),atmost(N,R,C2,L)):- !,
	arrange(C,C2).
arrange(atleast(N,R,C,[Original|Sel]),atleast(N,R,C2,[Original2|Sel])):- !,
	arrange(C,C2),
	arrange(Original,Original2).
arrange(C,C).


% List lista csokkeno sorrendbe rendezese Arranged
arrangeList([C],[C2]):- !,
	arrange(C,C2).	
arrangeList(List,Arranged):-
	length(List,L),
	L1 is L // 2,
	length(List1,L1),
	append(List1,List2,List),
	arrangeList(List1,Arranged1),
	arrangeList(List2,Arranged2),
	merge(Arranged1,Arranged2,Arranged).

% csokkeno sorrendbe rendezett listak osszefesulese
merge([],L,L):- !.
merge(L,[],L):- !.
merge([C|Cs],[C|Ds],[C|Rest]):- !,
	merge(Cs,Ds,Rest).
merge([C|Cs],[D|Ds],[Max|Rest]):-
	(
	  smaller(C,D) ->
	  Max = D,
	  merge([C|Cs],Ds,Rest)
	; Max = C,
	  merge(Cs,[D|Ds],Rest)
	).
	

% szerepek rendezese
smaller(arole(R),arole(S)):- !,
	R @< S.
smaller(inv(arole(R)),arole(S)):- !,
	R @=< S.
smaller(arole(R),inv(arole(S))):- !,
	R @< S.
smaller(inv(arole(R)),inv(arole(S))):- !,
	R @< S.

% diszjunkciot erinto rendezes
smaller(or(Cs),or(Ds)):- !,
	arrangeList(Cs,Cs2),
	arrangeList(Ds,Ds2),
	smaller_arranged_list(Cs2,Ds2).
smaller(or(Cs),D):- !,
	arrangeList(Cs,[C|_]),
	smaller(C,D).
smaller(C,or(Ds)):- !,
	arrangeList(Ds,[D|_]),
	(
	  C = D
	; smaller(C,D)
	), !.

% literalok rendezese
smaller(bottom,_):- !.
smaller(_,bottom):- !, fail.
smaller(top,_):- !.
smaller(_,top):- !, fail.

smaller(not(C),D):- !,
	smaller(C,D).
smaller(C,not(D)):- !,
	(
	  C = D, !
	; smaller(C,D)
	).

smaller(nconcept(C),nconcept(D)):- !,
	C @< D.
smaller(nconcept(_),_):- !.
smaller(_,nconcept(_)):- !, fail.
smaller(aconcept(C),aconcept(D)):- !,
	C @< D.
smaller(aconcept(_),_):- !.
smaller(_,aconcept(_)):- !, fail.
smaller(and(Cs),and(Ds)):- !,
	arrangeList(Cs,Cs2),
	arrangeList(Ds,Ds2),
	smaller_arranged_list(Cs2,Ds2).
smaller(and(_),_):- !.
smaller(_,and(_)):- !, fail.
smaller(atmost(N1,R1,C1,_), atmost(N2,R2,C2,_)):- !,
       (
	 smaller(R1,R2)
	 ; R1 = R2, N1 < N2
	 ; R1 = R2, N1 = N2, smaller(C1,C2)
       ), !.
smaller(atmost(_,_,_,_),_):- !.
smaller(_,atmost(_,_,_,_)):- !, fail.
smaller(atleast(_,_,_,Sel1), atleast(_,_,_,Sel2)):- !,
	length(Sel1,L1),
	length(Sel2,L2),
	L1 < L2.
       

% ket csokkeno sorba rendezett listabol az elso a kisebb
smaller_arranged_list([],_):- !.
smaller_arranged_list(_,[]):- !, fail.
smaller_arranged_list([C|Cs],[C|Ds]):- !,
	smaller_arranged_list(Cs,Ds).
smaller_arranged_list([C|Cs],[D|Ds]):- !,
	(
	  smaller(C,D), !
	; smaller_arranged_list(Cs,Ds)
	).