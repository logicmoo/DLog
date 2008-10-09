:- module(saturate_pure,[saturate/3,saturate_partially/4,simplifyConcept/2]).

:- use_module('bool_pure', [boolneg/2, boolinter/2, boolunion/2]).
:- use_module('transitive_pure', [inv/2]).
:- use_module('selectResolvable_pure', [selectResolvableList/2, selectResolvable/2, greater/2]).
:- use_module('../struct', [contains_struct/2]).	    
:- use_module(library(lists),[append/3,member/2,select/3,delete/3]).
:- use_module(library(ordsets),[list_to_ord_set/2,ord_subset/2]).

% :- use_module('../prolog_translator/prolog_translator_swi_tools', [bb_put/2, bb_get/2]).

/*********************** Fogalomhalmaz telitese **************************/

% saturate(+W,+RInclusion,-S):-
% W SHIQ fogalmak listaja, melyek telitesevel kapjuk S-et
% RInclusion szerephierarchia mellett
% bejarati hivas
saturate(W,RInclusion,S):-
	simplifyConcepts(W,W2),
	selectResolvableList(W2,Selected),
	saturate([],Selected,RInclusion,S).

% saturate_partially(+W1,+W2,+RInclusion,+S):-
% Telitjuk W1 unio W2-t, de ugy, hogy W1 belieket nem rezolvalunk egymassal
% az eredmeny S
saturate_partially(W1,W2,RInclusion,S):-
	simplifyConcepts(W2,Simplified),
	selectResolvableList(Simplified,Selected),
	saturate(W1,Selected,RInclusion,S).

/********************************saturation*****************************/

% saturate(+W1,+W2,+RInclusion,-S): W1 es W2 "rendezett fogalmak" listaja,
% es RInclusion tartalmazza a szerephierarchiat. S-t W1 es W2
% telitesevel nyerjuk ugy hogy W1 es W2-beli fogalmakat, valamint W2-W2-beli
% fogalmakat rezolvalunk egymassal
saturate(W1,[],_,W1):- !.
saturate(W1,[C|W2],RInclusion,S):-
	redundant(C,W1), !,
	% print('---- ') ,print(C), print('---- redundans'),
	saturate(W1,W2,RInclusion,S).
saturate(W1,[C|W2],RInclusion,S):-
	% nl, print(C),
	findall(R,(
		   (
		     member(A,W1),
		     resolve(C,A,R1)
		   ; resolve_hierarchy(C,RInclusion,R1)
		   ; resolve_alone(C,R1)
		   ),		     
		   simplifyConcept(R1,R2),
		   selectResolvable(R2,R),
		   % nl, print('  = '), print(R),
		   true
		  ), Rs),
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


% includes(+C,+D): C fogalom tartalmazza D-t, azaz C redundans D miatt
includes(_,bottom):- !.
includes(top,_):- !.
includes(X,X):- !.

includes(and([C]),and(D)):- !,
	member(C,D).
includes(and([C|Cs]),and(D)):- !,
	append(_,[C|DRest],D),
	includes(and(Cs),and(DRest)).
includes(C,and(Ds)):- !,
	member(C,Ds).
	
includes(atmost(N,R,C),atmost(K,R,D)):- !,
	K =< N,
	includes(D,C).
includes(atleast(N,R,C),atleast(K,R,D)):- !,
	N =< K,
	includes(C,D).

includes(or(_),or([])):- !.
includes(or(C),or([D|Ds])):- !,
	member(X,C),
	includes(X,D), !,
	includes(or(C),or(Ds)).
includes(or(C),D):- !,
	member(X,C),
	includes(X,D).

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

% resolve_hierarchy(+C,+RInclusion,-Res):- C fogalom rezolvalhato az
% egyik RInclusion-beli szereptartalmazasi axiomaval. Az eredmeny Res.
% 7. szabaly
resolve_hierarchy(atleast(N,R,C),RInclusion,atleast(N,S,C)):- !,
	(
	  member(subrole(R,S),RInclusion), !
	; member(subrole(R1,S1),RInclusion),
	  inv(R1,R), inv(S1,S)
	).
resolve_hierarchy(or([C|Rest]),RInclusion,or([D|Rest])):-
	resolve_hierarchy(C,RInclusion,D).


% 6. szabaly
resolve_alone(or([atmost(0,R,C1)|Rest]),Res):-
	select(atmost(0,R,C2),Rest,Rest2), !,
	boolinter([C1,C2],C),
	(
	  Rest2 = [] -> Res = atmost(0,R,C)
	; Res = or([atmost(0,R,C)|Rest2])
	).



% resolve(+C1,+C2,-R):- C1 es C2 fogalmak rezolvense R
% 5. szabaly - diszjunkcioval
resolve(or([atmost(0,R,C)|D]),atleast(K,S,F),Res):-
	inv(R,S), !,
	\+ contains_struct(D,atmost(_,_,_)),
	boolneg(C,NC),
	boolunion(D,DUnion),
	boolinter([F,DUnion],New),
	boolunion([atleast(K,S,New),NC],Res).
resolve(or([atmost(0,R,C)|D]),or([atleast(K,S,F)|E]),Res):-
	inv(R,S), !,
	\+ contains_struct(D,atmost(_,_,_)),
	boolneg(C,NC),
	boolunion(D,DUnion),
	boolinter([F,DUnion],New),
	boolunion([atleast(K,S,New),NC|E],Res).
resolve(atleast(K,S,F),or([atmost(0,R,C)|D]),Res):-
	inv(R,S), !,
	\+ contains_struct(D,atmost(_,_,_)),
	boolneg(C,NC),
	boolunion(D,DUnion),
	boolinter([F,DUnion],New),
	boolunion([atleast(K,S,New),NC],Res).
resolve(or([atleast(K,S,F)|E]),or([atmost(0,R,C)|D]),Res):-
	inv(R,S), !,
	\+ contains_struct(D,atmost(_,_,_)),
	boolneg(C,NC),
	boolunion(D,DUnion),
	boolinter([F,DUnion],New),
	boolunion([atleast(K,S,New),NC|E],Res).

% 2. szabaly
resolve(atleast(N,R,C),D,atleast(N,R,CD)):-
	\+ contains_struct(D,arole(_)), !,
	resolve(C,D,E),
	boolinter([C,E],CD).
resolve(D, atleast(N,R,C),atleast(N,R,CD)):-
	\+ contains_struct(D,arole(_)), !,
	resolve(C,D,E),
	boolinter([C,E],CD).
resolve(or([atleast(N,R,C)|Rest]),D,or([atleast(N,R,CD)|Rest])):-
	\+ contains_struct(D,arole(_)), !,
	resolve(C,D,E),
	boolinter([C,E],CD).
resolve(D, or([atleast(N,R,C)|Rest]),or([atleast(N,R,CD)|Rest])):-
	\+ contains_struct(D,arole(_)), !,
	resolve(C,D,E),
	boolinter([C,E],CD).


% diszjunktiv fogalmak kezelese
resolve(or([C|Cs]),D,or(R)):- !,
	resolve(C,D,CDRES),
	(
	  CDRES = or(CDRES1) -> append(CDRES1,Cs,R)
	; R = [CDRES|Cs]
	).
resolve(D,or([C|Cs]),or(R)):- !,
	resolve(C,D,CDRES),
	(
	  CDRES = or(CDRES1) -> append(CDRES1,Cs,R)
	; R = [CDRES|Cs]
	).

% nincs tobb diszjunkcio

% 5. szabaly
resolve(atmost(0,R,C),atleast(_,S,_),Res):-
	inv(R,S), !,
	boolneg(C,Res).
resolve(atleast(_,S,_),atmost(0,R,C),Res):-
	inv(R,S), !,
	boolneg(C,Res).


% 3-4. szabaly
resolve(atmost(N,R,C),atleast(K,R,D),Res):- !,
	boolneg(C,NotC),
	boolneg(D,NotD),
	boolinter([C,NotD],CNotD),
	boolinter([D,NotC],DNotC),
	(
	  N < K -> % 3. szabaly
	  K1 is K - N,
	  Res = atleast(K1,R,DNotC)
	; % N >= K   4. szabaly
	  N1 is N - K,
	  Res = or([atmost(N1,R,CNotD),atleast(1,R,DNotC)])
	).
resolve(atleast(K,R,D),atmost(N,R,C),Res):- !,
	boolneg(C,NotC),
	boolneg(D,NotD),
	boolinter([C,NotD],CNotD),
	boolinter([D,NotC],DNotC),
	(
	  N < K -> % 3. szabaly
	  K1 is K - N,
	  Res = atleast(K1,R,DNotC)
	; % N >= K   4. szabaly
	  N1 is N - K,
	  Res = or([atmost(N1,R,CNotD),atleast(1,R,DNotC)])
	).


% 1. szabaly
resolve(and(Cs),and(Ds),bottom):- !,
	member(C,Cs),
	member(not(C),Ds), !.
resolve(and(Cs),D,bottom):- !,
	member(C,Cs),
	(
	  C = not(D)
	; D = not(C)
	), !.
resolve(D,and(Cs),bottom):- !,
	member(C,Cs),
	(
	  C = not(D)
	; D = not(C)
	), !.
resolve(not(C),C,bottom):- !.
resolve(C,not(C),bottom):- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
simplifyConcept(atleast(N,R,C),Simplified):-
	!,
	simplifyConcept(C,C2),
	(
	  N < 1 -> Simplified = top
	; C2 == bottom -> Simplified = bottom
	; Simplified = atleast(N,R,C2)
	).

simplifyConcept(or(C),Simplified):-
	!,
	simplifyConcepts(C,C2),
	(
	  member(top,C2) -> Simplified = top
	; member(A,C2), member(not(A),C2) -> Simplified = top
	; simplifyDisjunction(C2,C3), sort(C3,C4),
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
	; delete(C2,top,C3), sort(C3,C4),
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

simplifyDisjunction([],Ls,Ls).
simplifyDisjunction([C|Cs],L,Rs):-
	(
	  member(X,L), includes(X,C) -> simplifyDisjunction(Cs,L,Rs)
	; member(X,Cs), includes(X,C) -> simplifyDisjunction(Cs,L,Rs)
	; simplifyDisjunction(Cs,[C|L],Rs)
	).