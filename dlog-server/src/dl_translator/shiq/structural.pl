:- module(structural, [def_list/2,literal_concept/1]).
:- use_module('nnf',[neg/2]).
:- use_module(library(lists)).
/*******************************************************/
/************* Strukturalis transzformacio  ************/
/*******************************************************/

% def_list(+Cs,-DCs): DCs a Cs listaban talalhato
% fogalmak egyertelmuen definialt formaibol allo lista
def_list([],[]).
def_list([C|Cs],Defs):-
	def(C,Defs1),
	append(Defs1,Defs2,Defs),
	def_list(Cs,Defs2).

% def(+C, -Defs):
% C egy NNF ALCHIQ fogalom
% Defs pedig Def(C) elemeibol allo lista
def(C,_,[C]):-
	not_embedded(C), !.
def(atleast(N,R,C),[atleast(N,R,Q)|Defs]):-
	def2(C,1,Q,Defs).
def(atmost(N,R,C),[atmost(N,R,Q)|Defs]):-
	def2(C,-1,Q,Defs).
def(and(L),Defs):-
	select(C,L,Rest), \+ literal_concept(C), !,	
	def2(C,1,Q,Defs1),
	append(Defs1,Defs2,Defs),
	def(and([Q|Rest]),Defs2).
def(or(L),Defs):-
	findall(A,(
		   member(A,L),
		   \+ literal_concept(A)
		  ), Cs
	       ),
	% ha csak egy beagyazott fogalom van, akkor nem kell uj valtozo
	(
	  Cs = [C,_|_] ->
	  select(C,L,Rest),
	  def2(C,1,Q,Defs1),
	  append(Defs1,Defs2,Defs),	  
	  def(or([Q|Rest]),Defs2)
	; Cs = [C],
	  select(C,L,LRest),
	  def2(C,1,Q,Defs1),
	  neg(Q,NQ),
	  select(or(D),Defs1,Rest1), select(NQ,D,DRest), !,
	  append(DRest,LRest,DL),
	  Defs = [or(DL)|Rest1]
	).


% def2(+C, +Pol, -Q, -Defs):
% C egy NNF ALCHIQ fogalom
% Defs pedig Def(C) elemeibol allo lista
% Pol jelzi az adott reszfogalom polaritasat
% C-t magat is helyettesitjuk a Q uj fogalomnevvel
def2(C,Pol,Q,Defs):-
	not_embedded(C), !,
	defName(C,Q),
	(
	  Pol = 1 -> (
		       C = or(Cs) -> Defs = [or([not(Q)|Cs])], !
		     ; Defs = [or([not(Q),C])]
		     )
	; Pol = -1 -> (
			C = or(Cs) ->
			Defs = [or([Q,and(NCs)])],
			neglist(Cs,NCs)
		      ; neg(C,NC), Defs = [or([Q,NC])]
		      )
	).
def2(atleast(N,R,C),Pol,Q1,[D|Defs]):-
	def2(C,Pol,Q2,Defs),
	def2(atleast(N,R,Q2),Pol,Q1,[D]).
def2(atmost(N,R,C),Pol,Q1,[D|Defs]):-
	Pol1 is Pol * -1,
	def2(C,Pol1,Q2,Defs),
	def2(atmost(N,R,Q2),Pol,Q1,[D]).
def2(and(L),1,Q1,Defs):-
	select(C,L,Rest), \+ literal_concept(C), !,	
	def2(C,1,Q2,Defs1),
	append(Defs1,Defs2,Defs),
	def2(and([Q2|Rest]),1,Q1,Defs2).
def2(or(L),-1,Q1,Defs):-
	select(C,L,Rest), \+ literal_concept(C), !,	
	def2(C,-1,Q2,Defs1),
	append(Defs1,Defs2,Defs),
	def2(or([Q2|Rest]),-1,Q1,Defs2).
def2(AndOr,Pol,Q1,Defs):-
	( AndOr = and(L) ; AndOr = or(L) ), !,
	findall(A,(
		   member(A,L),
		   \+ literal_concept(A)
		  ), Cs
	       ),
	(
	  Cs = [C,_|_] ->
	  select(C,L,Rest),
	  def2(C,Pol,Q2,CDefs),
	  append(CDefs,RestDefs,Defs),
	  (
	    AndOr = and(L) -> def2(and([Q2|Rest]),Pol,Q1,RestDefs)
	  ; AndOr = or(L) -> def2(or([Q2|Rest]),Pol,Q1,RestDefs)
	  )
	; Cs = [C],
	  select(C,L,LRest),
	  def2(C,Pol,Q2,CDefs),
	  neg(Q2,NQ2),
	  select(or(D),CDefs,CDefsRest), select(NQ2,D,DRest), !,
	  append(DRest,LRest,NewL),
	  (
	    AndOr = or(L) ->
	    defName(or(NewL),Q1),
	    Defs = [or([not(Q1)|NewL])|CDefs]
	  ; AndOr = and(L) ->
	    defName(and(NewL),Q1),
	    neg(and(NewL),or(NegNewL)), %TODO ez ugye neg? 
	    Defs = [or([Q1|NegNewL])|CDefsRest]
	  )
	).


% neglist(L,NL):- Az L lista fogalmainak negaltjaibol all NL lista.
neglist([],[]).
neglist([L|Ls],[NL|NLs]):-
	neg(L,NL),
	neglist(Ls,NLs).

% literal_concept(+C): igaz, ha C literal fogalom, feltetelezve, hogy C nnf formaju
literal_concept(C):-
	C=aconcept(X);C=not(X);C=top;C=bottom;C=nconcept(X).

% literal_list(+L):- L literalok listaja
literal_list([]).
literal_list([C|L]):-
	literal_concept(C),
	literal_list(L).

% not_embedded(+C): C fogalom nem tartalmaz beagyazott osszetett fogalmat
not_embedded(and(L)):-
	literal_list(L), !.
not_embedded(or(L)):-
	literal_list(L), !.
not_embedded(atleast(_,_,C)):-
	literal_concept(C), !.
not_embedded(atmost(_,_,C)):-
	literal_concept(C), !.
not_embedded(C):-
	literal_concept(C), !.

% defName(+Concept,-nconcept(Name)):- Concept fogalomhoz egy
% egyedi fogalom Name,
defName(C,nconcept(N)):-
	defName2(C,N).

% defName2(+Concept,-Name):- Concept fogalomhoz egy
% egyedi string Name
defName2(top,top).
defName2(bottom,bottom).
defName2(aconcept(C),C).
defName2(nconcept(C),C).
defName2(arole(R),R).
defName2(inv(R),N):-
	defName2(R,RN),
	atom_concat(inv_,RN,N).
defName2(not(C),N):-
	defName2(C,N1),
	atom_concat(not_,N1,N).
defName2(atleast(Num,R,C),N):-
	number_codes(Num,NumCode), atom_codes(Num1,NumCode),
	atom_concat(atleast_,Num1,N1),
	atom_concat(N1,'_',N2),
	defName2(R,N3),
	atom_concat(N2,N3,N4),
	atom_concat(N4,'_',N5),
	defName2(C,N6),
	atom_concat(N5,N6,N).
defName2(atmost(Num,R,C),N):-
	number_codes(Num,NumCode), atom_codes(Num1,NumCode),	
	atom_concat(atmost_,Num1,N1),
	atom_concat(N1,'_',N2),
	defName2(R,N3),
	atom_concat(N2,N3,N4),
	atom_concat(N4,'_',N5),
	defName2(C,N6),
	atom_concat(N5,N6,N).
defName2(and([]),'').
defName2(and([C|L]),N):-
	defName2(C,N1),
	atom_concat(and_,N1,N2),
	atom_concat(N2,'_',N3),
	defName2(and(L),N4),
	atom_concat(N3,N4,N).
defName2(or([]),'').
defName2(or([C|L]),N):-
	defName2(C,N1),
	atom_concat(or_,N1,N2),
	atom_concat(N2,'_',N3),
	defName2(or(L),N4),
	atom_concat(N3,N4,N).

