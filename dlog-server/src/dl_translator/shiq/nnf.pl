:- module(nnf,[neg/2,axiomsToNNFConcepts/2,negNormForm/2]).

:- use_module(library(lists)).


% neg(+Q,-NQ):- Q fogalom negaltja NQ
neg(not(X),X):- !.
neg(X,Y):-
	negNormForm(not(X),Y).



% axiomsToNNFConcepts(+Axioms, -Concepts): Concepts az Axioms
% listaban levo axiomak belsositesei negacios normalformainak listaja
% Axioms csak fogalomtartalmazasi axiomakbol all
axiomsToNNFConcepts([implies(C1, C2)|As], [N|Cs]):-
	negNormForm(or([not(C1),C2]),N),	
	axiomsToNNFConcepts(As, Cs).
axiomsToNNFConcepts([],[]).

% negNormForm(+Concept, -NegForm): NegForm Concept fogalom negacios
% normalformaju megfeleloje
negNormForm(not(top),bottom):- !.
negNormForm(not(bottom),top):- !.
negNormForm(not(not(X)),N):-
	!,negNormForm(X,N).
negNormForm(not(and(Xs)),N):-
	!,negate_negNormForm_list(Xs,Ns),
	simplifyAndOr(or(Ns),N).
negNormForm(not(or(Xs)),N):-
	!,negate_negNormForm_list(Xs,Ns),
	simplifyAndOr(and(Ns),N).
negNormForm(not(all(R,C)),atleast(1,R,NC)):-
	!,negNormForm(not(C),NC).
negNormForm(not(some(R,C)),atmost(0,R,C)):- !.
negNormForm(not(atleast(N,R,C)),atmost(N2,R,C)):-
	!, N2 is N - 1.
negNormForm(not(atmost(N,R,C)),atleast(N2,R,C)):-
	!, N2 is N + 1.

negNormForm(and(Xs), N):-
	!, negNormForm_list(Xs,Ns2),
	simplifyAndOr(and(Ns2),N).	
negNormForm(or(Xs), N):-	
	!, negNormForm_list(Xs,Ns2),
	simplifyAndOr(or(Ns2),N).		
negNormForm(some(R,C), atleast(1,R,C2)):-
	!,negNormForm(C, C2).
negNormForm(all(R,C), atmost(0,R,NC)):-
	!,negNormForm(not(C), NC).
negNormForm(atleast(N,R,C), atleast(N,R,C2)):-
	!,negNormForm(C, C2).
negNormForm(atmost(N,R,C), atmost(N,R,C2)):-
	!,negNormForm(C, C2).
negNormForm(X,X).

% negNormForm_list(+L,-NL): NL az L listaban talalhato fogalmak
% negacios normalformainak a listaja
negNormForm_list([],[]).
negNormForm_list([C|Cs],[N|Ns]):-
	negNormForm(C,N),
	negNormForm_list(Cs,Ns).

% negate_negNormForm_list(+L,-NL): NL az L listaban talalhato fogalmak
% negaltjai negacios normalformainak a listaja
negate_negNormForm_list([],[]).
negate_negNormForm_list([C|Cs],[N|Ns]):-
	negNormForm(not(C),N),
	negate_negNormForm_list(Cs,Ns).

simplifyAndOr(and([X]),X):- !.
simplifyAndOr(or([X]),X):- !.
simplifyAndOr(and(Xs),Ys):- !,
	( select(and(Zs),Xs,Xs2) ->
	    append(Zs,Xs2,Xs3),
	    simplifyAndOr(and(Xs3),Ys)
	; Ys = and(Xs)
	).
simplifyAndOr(or(Xs),Ys):-	
	( select(or(Zs),Xs,Xs2)	->
	    append(Zs,Xs2,Xs3),
	    simplifyAndOr(or(Xs3),Ys)
	; Ys = or(Xs)
	).