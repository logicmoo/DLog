:- module(bool_pure, [boolneg/2, boolunion/2, boolinter/2]).
:- use_module('dl_to_fol_pure',[negNormForm/2]).
:- use_module('saturate_pure',[simplifyConcept/2]).
:- use_module(library(lists), [append/3, member/2]).

boolneg(not(C),C):- !.
boolneg(C,NC):-
	negNormForm(not(C),C1),
	normalize(C1,NC1),
	simplifyConcept(NC1,NC).

boolunion(Cs,D):-
	normalize(or(Cs),D1),
	simplifyConcept(D1,D).

boolinter(Cs,D):-
	normalize(and(Cs),D1),
	simplifyConcept(D1,D).
	

normalize(C,D):-
	findall( X, (
		      one_disjunct(C,X)
		    ), Xs
	       ),
	(
	  Xs = [D], !
	; D = or(Xs)
	).

one_disjunct(or(L),D):- !,
	member(C,L),
	one_disjunct(C,D).
one_disjunct(and([C]),D):- !,
	one_disjunct(C,D).
one_disjunct(and([C|Cs]),X):- !,
	one_disjunct(C,D),
	one_disjunct(and(Cs),Ds),
	(
	  D = and(D1) ->
	  (
	    Ds = and(Ds1) ->
	    append(D1,Ds1,X1),
	    X = and(X1)
	  ; X = and([Ds|D1])
	  )
	; (
	    Ds = and(Ds1) ->
	    X = and([D|Ds1])
	  ; X = and([D,Ds])
	  )
	).
	
	
one_disjunct(X,X).