:- module(dl_to_fol, [axiomsToNNFConcepts/2, def_list/3, negNormForm/2]).

:- use_module(library(lists)).

/***************************************************/
/********* Negacios normalformara hozas ************/
/***************************************************/

% axiomsToNNFConcepts(+Axioms, -Concepts): Concepts az Axioms
% listaban levo axiomak belsositesei negacios normalformainak listaja
% Axioms csak fogalomtartalmazasi axiomakbol all
axiomsToNNFConcepts([implies(top, C2)|As], [N2|Cs]):-
	!, negNormForm(C2,N2),
	axiomsToNNFConcepts(As,Cs).
axiomsToNNFConcepts([implies(C1, bottom)|As], [N1|Cs]):-
	!, negNormForm(not(C1),N1),
	axiomsToNNFConcepts(As,Cs).
axiomsToNNFConcepts([implies(C1, C2)|As], [N|Cs]):-
	negNormForm(or([not(C1),C2]),N),	
	axiomsToNNFConcepts(As, Cs).
axiomsToNNFConcepts([],[]).

% negNormForm(+Concept, -NegForm): NegForm Concept fogalom negacios
% normalformaju megfeleloje
negNormForm(top, top):- !.
negNormForm(bottom, bottom):- !.
negNormForm(aconcept(X), aconcept(X)):- !.
negNormForm(nconcept(X), nconcept(X)):- !.
negNormForm(not(aconcept(X)),not(aconcept(X))):- !.
negNormForm(not(nconcept(X)),not(nconcept(X))):- !.
negNormForm(not(top),bottom):- !.
negNormForm(not(bottom),top):- !.

negNormForm(not(not(X)),N):-
	!,negNormForm(X,N).

negNormForm(not(and([X])),N):-
	!, negNormForm(not(X),N).
negNormForm(not(and(Xs)),or(Ns)):-
	!,nnegForms(Xs,Ns).

negNormForm(not(or([X])),N):-
	!, negNormForm(not(X),N).
negNormForm(not(or(Xs)),and(Ns)):-
	!,nnegForms(Xs,Ns).

negNormForm(not(all(R,C)),atleast(1,R,N,[N])):-
	!,negNormForm(not(C),N).
negNormForm(not(some(R,C)),atmost(0,R,C2,[])):-
	!,negNormForm(C,C2).	
negNormForm(not(atleast(N,R,C)),atmost(N2,R,C2,[])):-
	!,negNormForm(C,C2),
	N2 is N - 1.	
negNormForm(not(atleast(N,R,C,_)),atmost(N2,R,C2,[])):-
	!,negNormForm(C,C2),
	N2 is N - 1.	
negNormForm(not(atmost(N,R,C)),atleast(N2,R,C2,[C2])):-
	!,negNormForm(C,C2),
	N2 is N + 1.
negNormForm(not(atmost(N,R,C,_)),atleast(N2,R,C2,[C2])):-
	!,negNormForm(C,C2),
	N2 is N + 1.

negNormForm(and([X]),N):-
	!, negNormForm(X,N).
negNormForm(and(Xs), and(Ns)):-
	!, negForms(Xs,Ns2),
	simplifyAnd(Ns2,Ns).	
negNormForm(or([X]),N):-
	!, negNormForm(X,N).
negNormForm(or(Xs), or(Ns)):-	
	!, negForms(Xs,Ns2),
	simplifyOr(Ns2,Ns).		

negNormForm(some(R,C), atleast(1,R,N,[N])):-
	!,negNormForm(C, N).

negNormForm(all(R,C), atmost(0,R,N,[])):-
	!,negNormForm(not(C), N).
	
negNormForm(atleast(N,R,C), atleast(N,R,C2,[C2])):-
	!,negNormForm(C, C2).
negNormForm(atleast(N,R,C,Sel), atleast(N,R,C2,Sel)):-
	!,negNormForm(C, C2).

negNormForm(atmost(N,R,C), atmost(N,R,C2,[])):-
	!,negNormForm(C, C2).
negNormForm(atmost(N,R,C,L), atmost(N,R,C2,L)):-
	!,negNormForm(C, C2).


% negForms(+L,-NL): NL az L listaban talalhato fogalmak
% negacios normalformainak a listaja
negForms([],[]).
negForms([C|Cs],[N|Ns]):-
	negNormForm(C,N),
	negForms(Cs,Ns).

% nnegForms(+L,-NL): NL az L listaban talalhato fogalmak
% negaltjai negacios normalformainak a listaja
nnegForms([],[]).
nnegForms([C|Cs],[N|Ns]):-
	negNormForm(not(C),N),
	nnegForms(Cs,Ns).


%simplifyAnd(+Xs,-Ys)
simplifyAnd(Xs,Ys):-	
	( select(and(Zs),Xs,Xs2) ->
	    append(Zs,Xs2,Xs3),
	    simplifyAnd(Xs3,Ys)
	; Ys = Xs
	).
	
%simplifyOr(+Xs,-Ys)
simplifyOr(Xs,Ys):-	
	( select(or(Zs),Xs,Xs2)	->
	    append(Zs,Xs2,Xs3),
	    simplifyOr(Xs3,Ys)
	; Ys = Xs
	).
	
/*******************************************************/
/************* Strukturalis transzformacio  ************/
/*******************************************************/

% def_list(+Cs,+Prefix,-DCs): DCs a Cs listaban talalhato
% fogalmak egyertelmuen definialt formaibol allo lista es az uj
% fogalomneveket Prefix elotaggal latjuk el
def_list([],_,[]).
def_list([C|Cs],Prefix,Defs):-
	def(C,Prefix,Defs1),
	append(Defs1,Defs2,Defs),
	def_list(Cs,Prefix,Defs2).

% def(+C, +Prefix, -Defs):
% C egy NNF ALCHIQ fogalom
% Defs pedig Def(C) elemeibol allo lista
% az uj fogalomnevek elotagja Prefix
def(C,_,[C]):-
	not_embedded(C), !.
def(atleast(N,R,C,Sel),Prefix,[atleast(N,R,Q,Sel)|Defs]):-
	def2(C,Prefix,1,Q,Defs).
def(atmost(N,R,C,L),Prefix,[atmost(N,R,Q,L)|Defs]):-
	def2(C,Prefix,-1,Q,Defs).
def(and(L),Prefix,Defs):-
	select(C,L,Rest), \+ literal_concept(C), !,	
	def2(C,Prefix,1,Q,Defs1),
	append(Defs1,Defs2,Defs),
	def(and([Q|Rest]),Prefix,Defs2).
def(or(L),Prefix,Defs):-
	findall(A,(
		   member(A,L),
		   \+ literal_concept(A)
		  ), Cs
	       ),
	% ha csak egy beagyazott fogalom van, akkor nem kell uj valtozo
	(
	  Cs = [C,_|_] ->
	  select(C,L,Rest), !,
	  def2(C,Prefix,1,Q,Defs1),
	  append(Defs1,Defs2,Defs),	  
	  def(or([Q|Rest]),Prefix,Defs2)
	; Cs = [C],
	  select(C,L,LRest), !,
	  def2(C,Prefix,1,Q,Defs1),
	  neg(Q,NQ),
	  select(or(D),Defs1,Rest1), select(NQ,D,DRest), !,
	  append(DRest,LRest,DL),
	  Defs = [or(DL)|Rest1]
	).


% def2(+C, +Prefix, +Pol, -Q, -Defs):
% C egy NNF ALCHIQ fogalom
% Defs pedig Def(C) elemeibol allo lista
% az uj fogalomnevek elotagja Prefix
% Pol jelzi az adott reszfogalom polaritasat
% C-t magat is helyettesitjuk a Q uj fogalomnevvel
def2(C,Prefix,Pol,Q,Defs):-
	not_embedded(C), !,
	defName(C,Prefix,Q),
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
def2(atleast(N,R,C,Sel),Prefix,Pol,Q1,[D|Defs]):-
	def2(C,Prefix,Pol,Q2,Defs),
	def2(atleast(N,R,Q2,Sel),Prefix,Pol,Q1,[D]).
def2(atmost(N,R,C,L),Prefix,Pol,Q1,[D|Defs]):-
	Pol1 is Pol * -1,
	def2(C,Prefix,Pol1,Q2,Defs),
	def2(atmost(N,R,Q2,L),Prefix,Pol,Q1,[D]).
def2(and(L),Prefix,1,Q1,Defs):-
	select(C,L,Rest), \+ literal_concept(C), !,	
	def2(C,Prefix,1,Q2,Defs1),
	append(Defs1,Defs2,Defs),
	def2(and([Q2|Rest]),Prefix,1,Q1,Defs2).
def2(or(L),Prefix,-1,Q1,Defs):-
	select(C,L,Rest), \+ literal_concept(C), !,	
	def2(C,Prefix,-1,Q2,Defs1),
	append(Defs1,Defs2,Defs),
	def2(or([Q2|Rest]),Prefix,-1,Q1,Defs2).
def2(AndOr,Prefix,Pol,Q1,Defs):-
	( AndOr = and(L) ; AndOr = or(L) ), !,
	findall(A,(
		   member(A,L),
		   \+ literal_concept(A)
		  ), Cs
	       ),
	(
	  ( Cs = [C,_|_], !; Cs = [C], Pol = -1) ->
	  select(C,L,Rest),
	  def2(C,Prefix,Pol,Q2,CDefs),
	  append(CDefs,RestDefs,Defs),
	  (
	    AndOr = and(L) -> def2(and([Q2|Rest]),Prefix,Pol,Q1,RestDefs)
	  ; AndOr = or(L) -> def2(or([Q2|Rest]),Prefix,Pol,Q1,RestDefs)
	  )
	; Cs = [C],
	  select(C,L,LRest),
	  def2(C,Prefix,Pol,Q2,CDefs),
	  neg(Q2,NQ2),
	  select(or(D),CDefs,CDefsRest), select(NQ2,D,DRest), !,
	  append(DRest,LRest,NewL),	  
	  (
	    AndOr = or(L) ->
	    defName(or(NewL),Prefix,Q1),
	    Defs = [or([not(Q1)|NewL])|CDefs]
	  ; AndOr = and(L) ->
	    defName(and(NewL),Prefix,Q1),
	    neg(and(NewL),or(NegNewL)), %TODO ez ugye neg? 
	    Defs = [or([Q1|NegNewL])|CDefsRest]
	  )
	).
	  
% neg(+Q,-NQ):- Q fogalom negaltja NQ
neg(not(X),X):- !.
neg(X,Y):-
	negNormForm(not(X),Y).

% neglist(L,NL):- Az L lista fogalmainak negaltjaibol all NL lista.
neglist([],[]).
neglist([L|Ls],[NL|NLs]):-
	neg(L,NL),
	neglist(Ls,NLs).

% literal_concept(+C): igaz, ha C literal fogalom, feltetelezve, hogy C nnf formaju
literal_concept(C):-
	C=aconcept(X);C=not(X);C=top;C=bottom;C=nconcept(X), !.

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
not_embedded(atleast(_,_,C,_)):-
	literal_concept(C), !.
not_embedded(atmost(_,_,C,_)):-
	literal_concept(C), !.
not_embedded(C):-
	literal_concept(C), !.

% defName(+Concept,+Prefix,-Name):- Concept fogalomhoz egy
% egyedi fogalom Name, melynek nevenek eleje Prefix
defName(C,Prefix,nconcept(N)):-
	defName(C,N1),
	atom_concat(Prefix,N1,N).

% defName(+Concept,-Name):- Concept fogalomhoz egy
% egyedi string Name
defName(top,top).
defName(bottom,bottom).
defName(aconcept(C),C).
defName(nconcept(C),C).
defName(arole(R),R).
defName(inv(R),N):-
	defName(R,N1),
	atom_concat(inv_,N1,N).
defName(not(C),N):-
	defName(C,N1),
	atom_concat(not_,N1,N).
defName(some(R,C),N):-
	defName(R,N1),
	atom_concat(some_,N1,N2),
	atom_concat(N2,'_',N3),
	defName(C,N4),
	atom_concat(N3,N4,N).
defName(all(R,C),N):-
	defName(R,N1),
	atom_concat(all_,N1,N2),
	atom_concat(N2,'_',N3),
	defName(C,N4),
	atom_concat(N3,N4,N).
defName(atleast(Num,R,C,_),N):-
	number_codes(Num,NumCode), atom_codes(Num1,NumCode),
	atom_concat(atleast_,Num1,N1),
	atom_concat(N1,'_',N2),
	defName(R,N3),
	atom_concat(N2,N3,N4),
	atom_concat(N4,'_',N5),
	defName(C,N6),
	atom_concat(N5,N6,N).
defName(atmost(Num,R,C,_),N):-
	number_codes(Num,NumCode), atom_codes(Num1,NumCode),	
	atom_concat(atmost_,Num1,N1),
	atom_concat(N1,'_',N2),
	defName(R,N3),
	atom_concat(N2,N3,N4),
	atom_concat(N4,'_',N5),
	defName(C,N6),
	atom_concat(N5,N6,N).
defName(and([]),'').
defName(and([C|L]),N):-
	defName(C,N1),
	atom_concat(and_,N1,N2),
	atom_concat(N2,'_',N3),
	defName(and(L),N4),
	atom_concat(N3,N4,N).
defName(or([]),'').
defName(or([C|L]),N):-
	defName(C,N1),
	atom_concat(or_,N1,N2),
	atom_concat(N2,'_',N3),
	defName(or(L),N4),
	atom_concat(N3,N4,N).