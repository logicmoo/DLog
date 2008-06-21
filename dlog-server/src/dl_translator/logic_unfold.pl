:- module(logic_unfold,[logic_unfold/2]).

:- use_module(toFOL, [redundant/2, elim_reds/3]).

logic_unfold(L,R):-
	saturate([],L,R).

% saturate(+W1,+W2,-R): W1 es W2 klozok listaja, R-t W1 es W2
% altalanos rezolucios telitesevel nyerjuk ugy hogy W1 es W2-beli klozokat,
% valamint W2-W2-beli klozokat rezolvalunk egymassal
saturate(W1,[],W1).
saturate(W1,[C|W2],R):-
	redundant(C,W1), !, 
	saturate(W1,W2,R).
saturate(W1,[C|W2],S):-
	% nl, print(C),
	findall(R,(
		   member(X,W1),
		   resolve(X,C,R)
		   ),Rs),
	elim_reds(W1,C,EW1),
	append(Rs,W2,EW2),
	saturate([C|EW1],EW2,S).		


resolve(C1,C2,R):-
	(
	  \+ member(not(arole(_,_,_)),C1)
	; \+ member(not(arole(_,_,_)),C2)
	),
	copy_term(C1,D1),
	copy_term(C2,D2),
	select(X1,D1,D1Rest),
	select(X2,D2,D2Rest),
	neglit(X1,X2), !,
	append(D1Rest,D2Rest,R1),
	sort(R1,R),
	\+ (
	     member(aconcept(Pred,A1),R), member(not(aconcept(Pred,A2)),R), A1 == A2
	   ; member(nconcept(Pred,A1),R), member(not(nconcept(Pred,A2)),R), A1 == A2
	   ).

% neglit(?C,?D):- C es D unaris literal fogalmak es egymas negaltjai
neglit(aconcept(C,X), not(aconcept(C,X))):- !.
neglit(nconcept(C,X), not(nconcept(C,X))):- !.
neglit(not(aconcept(C,X)), aconcept(C,X)):- !.
neglit(not(nconcept(C,X)), nconcept(C,X)):- !.