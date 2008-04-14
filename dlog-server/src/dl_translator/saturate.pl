:- module(saturate,[saturate/3,simplifyConcept/2]).

% :- use_module('../config').
:- use_module('bool', [boolneg/2, boolinter/2, boolunion/2]).
:- use_module('transitive', [inv/2]).
:- use_module(selectResolvable, [selectResolvableList/2, selectResolvable/2]).
:- use_module(library(lists),[append/3,member/2,select/3,delete/3]).
:- use_module(library(ordsets),[list_to_ord_set/2,ord_subset/2]).

/*********************** Fogalomhalmaz telitese **************************/

% saturate(+W,+RInclusion,-S):-
% W SHIQ fogalmak listaja, melyek telitesevel kapjuk S-et
% RInclusion szerephierarchia mellett
saturate(W,RInclusion,S):-
	simplifyConcepts(W,W2),
	selectResolvableList(W2,Selected),
	saturate([],Selected,RInclusion,S).


/********************************saturation*****************************/

% saturate(+W1,+W2,+RInclusion,-S): W1 es W2 "rendezett fogalmak" listaja,
% es RInclusion tartalmazza a szerephierarchiat. S-t W1 es W2
% telitesevel nyerjuk ugy hogy W1 es W2-beli fogalmakat, valamint W2-W2-beli
% fogalmakat rezolvalunk egymassal
saturate(W1,[],_,W1).
saturate(W1,[C|W2],RInclusion,S):-
	redundant(C,W1), !,
	nl, print('---- ') ,print(C), print('---- redundans'),
	saturate(W1,W2,RInclusion,S).
saturate(W1,[C|W2],RInclusion,S):-
	nl, print(C),
	findall(R,(
		   (
		     member(A,W1),
		     resolve(A,C,R1),
		     nl, print('  + '), print(A),
		     true
		   ; resolve2(C,RInclusion,R1)
		   ),		     
		   simplifyConcept(R1,R2),
		   selectResolvable(R2,R),
		   nl, print('  = '), print(R),
		   true
		  ), Rs),
% 	read(_),
	elim_reds(W1,C,EW1),
	append(Rs,W2,EW2),
	saturate([C|EW1],EW2,RInclusion,S).


/******************** Redundans klozok elhagyasa ***********************/

% redundant(+C, +Concepts): igaz, ha van C-nel szukebb fogalom a Concepts
% fogalomhalmazban
redundant(C,[D|Ds]):-
	(
	  includes(C,D), !
	; redundant(C,Ds)
	).

% includes(+C,+D): C fogalom tartalmazza D-t
includes(_,bottom):- !.
includes(top,_):- !.
includes(X,X):- !.
includes(aconcept(C), and(D)):- !,
	member(aconcept(C),D).
includes(not(aconcept(C)), and(D)):- !,
	member(not(aconcept(C)),D).
includes(nconcept(C), and(D)):- !,
	member(nconcept(C),D).
includes(not(nconcept(C)), and(D)):- !,
	member(not(nconcept(C)),D).

includes(and(C),and(D)):- !,
	list_to_ord_set(C,OC),
	list_to_ord_set(D,OD),
	ord_subset(OC,OD).
includes(atmost(N,R,C),atmost(K,R,D)):-
	K =< N,
	includes(D,C).
includes(atleast(N,R,C,Sel1),atleast(K,R,D,Sel2)):-
	(
	  append(Sel2,_,Sel1)
	; list_to_ord_set(Sel1,OrdSet),
	  list_to_ord_set(Sel2,OrdSet)
	),
	N =< K,
	includes(C,D).	

includes(or(C),or(D)):- !,
	includes_list_list(C,D).
includes(or(C),D):- !,
	includes_list(C,D,_).

% includes_list_list(+Cs,+Ds): Ds lista minden eleme megfeleltetheto
% Cs lista valamelyik elemenek
includes_list_list(_,[]):- !.
includes_list_list(C,[D|Ds]):-
	includes_list(C,D,DRed),
	select(DRed,C,Rest),
	includes_list_list(Rest,Ds).

% includes_list(+List,+C,-CRed): List lista CRed eleme redundans a
% C fogalom jelenleteben
includes_list([L|Ls],C,CRed):-
	(
	  includes(L,C), CRed = L
	; includes_list(Ls,C,CRed)
	).


% elim_reds(+Concepts,+C,-Reduced): Concepts fogalomhalmazbol
% C miatt redundans fogalmakat elhagyva kapjuk a Reduced halmazt
elim_reds([],_,[]).
elim_reds([A|Rest],C,Reduced):-
	redundant(A,[C]), !,
	elim_reds(Rest,C,Reduced).
elim_reds([A|Rest],C,[A|Reduced]):-
	elim_reds(Rest,C,Reduced).

/******************* Alap szuperpozicios kovetkeztetesi lepes *********************/
% resolve2(+C,+RInclusion,-R1):- C fogalom rezolvalhato az egyik RInclusion-beli
% szereptartalmazasi axiomaval
resolve2(atleast(N,R,C,Sel),RInclusion,atleast(N,S,C,Sel)):-
	member(subrole(R,S),RInclusion).
resolve2(or([atleast(N,R,C,Sel)|Rest]),RInclusion,or([atleast(N,S,C,Sel)|Rest])):-
	member(subrole(R,S),RInclusion).

% resolve(+C1,+C2,-R):- C1 es C2 fogalmak rezolvense R
% 1. szabaly
resolve(C,not(C),bottom):- !.
resolve(not(C),C,bottom):- !.
resolve(and(Cs),and(Ds),bottom):- !,
	member(C,Cs),
	member(D,Ds),
	boolneg(C,D), !.
resolve(and(Cs),D,bottom):-
	member(C,Cs),
	boolneg(C,D), !.
resolve(D,and(Cs),bottom):-
	member(C,Cs),
	boolneg(C,D), !.

% 5. szabaly
resolve(atleast(N,R,C,Sel),atleast(K,S,D,Sel2),atleast(M,R,CD,Sel)):- !,
	R = S,
	sameSelector(Sel,Sel2),
	resolve(C,D,CD),
	M is min(N,K).

% atleast mindig elolre kerul
resolve(D,atleast(N,R,C,Sel),Res):- !,
	resolve(atleast(N,R,C,Sel),D,Res).

% 2. szabaly
resolve(atleast(N,R,C,Sel),D,atleast(N,R,CD,Sel)):-
	\+ (
	     D = atleast(_,_,_,_)
	   ; D = atmost(_,_,_)
	   ; D = or([atleast(_,_,_,_)|_])
	   ; D = or([atmost(_,_,_)|_])
	   ), !,
	resolve(C,D,CD), !.

% 3., 4. szabaly
resolve(atleast(K,R,D,Sel),atmost(N,R,C),Res):- !,
	Sel = [Original],
	boolneg(C,NotC),
	boolneg(D,NotD),

	\+ includes(NotC,Original),
	
	boolinter([C,NotD],CNotD),
	boolinter([D,NotC],DNotC),
	(
	  K = 1 -> Sel2 = [Original, marked]
	; Sel2 = [Original,NotC]
	),
	(
	  N >= K ->
	  N1 is N - K,
	  Res = or([atmost(N1,R,CNotD),atleast(1,R,DNotC,Sel2)])
	; N1 is K - N,
	  Res = atleast(N1,R,DNotC,Sel2)
	).


% 7. szabaly
resolve(atleast(_,S,_,_),atmost(0,R,C),Res):- !,
	inv(R,S),
	boolneg(C,Res).

resolve(atleast(K,S,_,Sel),or(Cs),Res):-
	Cs = [atmost(0,R,_)|_], inv(R,S), !,
	findall( C, (
		      member(atmost(0,R,C1),Cs),
		      boolneg(C1,C)
		    ), Ci
	       ),
	findall( D, (
		      member(D,Cs),
		      \+ D = atmost(_,_,_)
		    ), Ds
	       ),
	boolunion(Ds,D2),
	boolunion([atleast(K,S,D2,Sel)|Ci],Res).


resolve(or(Cs),or([atleast(K,S,D,Sel)|Ds]),or(Res)):- !,
	resolve(atleast(K,S,D,Sel),or(Cs),R),
	(
	  R = or(R1) ->
	  append(R1,Ds,Res)
	; Res = [R|Ds]
	).

resolve(or([C|Cs]),D,or(R)):- !,
	resolve(C,D,CDRES),
	(
	  CDRES = or(CDRES1) ->
	  append(CDRES1,Cs,R)
	; R = [CDRES|Cs]
	).
resolve(D,or([C|Cs]),or(R)):- !,
	resolve(C,D,CDRES),
	(
	  CDRES = or(CDRES1) ->
	  append(CDRES1,Cs,R)
	; R = [CDRES|Cs]
	).
	
simplifyConcepts([],[]).
simplifyConcepts([C|Cs],[R|Rs]):-
	simplifyConcept(C,R),
	simplifyConcepts(Cs,Rs).

simplifyConcept(not(top), bottom):- !.
simplifyConcept(not(bottom), top):- !.

simplifyConcept(atmost(N,R,C),Simplified):-
	!,
	simplifyConcept(C,C2),
	(
	  C2 == bottom -> Simplified = top
	; Simplified = atmost(N,R,C2)
	).
simplifyConcept(atleast(N,R,C,Sel),Simplified):-
	!,
	simplifyConcept(C,C2),
	(
	  N < 1 -> Simplified = top
	; C2 == bottom -> Simplified = bottom
	; Simplified = atleast(N,R,C2,Sel)
	).

simplifyConcept(or(C),Simplified):-
	!,
	simplifyConcepts(C,C2),
	(
	  member(top,C2) -> Simplified = top
	; member(A,C2), member(not(A),C2) -> Simplified = top
	; sort(C2,C3),
	  simplifyDisjunction(C3,C4),
	  (
	    C4 = [] -> Simplified = bottom
	  ; C4 = [X] -> Simplified = X
	  ; Simplified = or(C4)
	  )
	).

simplifyConcept(and(C),Simplified):-
	!,
	simplifyConcepts(C,C2),
	(
	  member(bottom,C2) -> Simplified = bottom
	; member(A,C2), member(not(A),C2) -> Simplified = bottom
	; sort(C2,C3),
	  delete(C3,top,C4),
	  (
	    C4 = [] -> Simplified = top
	  ; C4 = [X] -> Simplified = X
	  ; Simplified = and(C4)
	  )
	).

simplifyConcept(C,C).


% simplifyDisjunction(+Cs,-Rs):- or(Cs) egyszerusitese or(Rs)
simplifyDisjunction(Cs,Rs):-
	simplifyDisjunction(Cs,[],Rs).

simplifyDisjunction([],Rs,Rs).
simplifyDisjunction([C|Cs],L,Rs):-
	(
	  member(X,L), includes(X,C) -> simplifyDisjunction(Cs,L,Rs)
	; member(X,Cs), includes(X,C) -> simplifyDisjunction(Cs,L,Rs)
	; simplifyDisjunction(Cs,[C|L],Rs)
	).

% sameSelector(+Sel1,+Sel2): Sel1 es Sel2 azonos szelektorok
sameSelector(X,X):- !.
sameSelector([X],[X,marked]):- !.
sameSelector([X,marked],[X]).