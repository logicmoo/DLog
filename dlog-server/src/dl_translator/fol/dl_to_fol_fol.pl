:- module(dl_to_fol_fol, [axiomsToNNFConcepts/2, def_list/3, toClauseList/2, list_cls/2, negNormForm/2, clause_types/2]).

:- use_module(library(lists)).
:- use_module('../struct').

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
negNormForm(top, top).
negNormForm(bottom, bottom).
negNormForm(aconcept(X), aconcept(X)).
negNormForm(nconcept(X), nconcept(X)).
negNormForm(not(aconcept(X)),not(aconcept(X))).
negNormForm(not(nconcept(X)),not(nconcept(X))).
negNormForm(not(top),bottom).
negNormForm(not(bottom),top).

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

negNormForm(not(all(R,C)),some(R,N)):-
	!,negNormForm(not(C),N).

negNormForm(not(some(R,C)),all(R,N)):-
	!,negNormForm(not(C),N).
	
negNormForm(not(atleast(1,R,C)),all(R,C2)):-
	!,negNormForm(not(C),C2).
	
negNormForm(not(atleast(N,R,C)),atmost(N2,R,C2)):-
	!,negNormForm(C,C2),
	N2 is N - 1.
	
negNormForm(not(atmost(0,R,C)),some(R,C2)):-
	!,negNormForm(C,C2).
	
negNormForm(not(atmost(N,R,C)),atleast(N2,R,C2)):-
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

negNormForm(some(R,C), some(R,N)):-
	!,negNormForm(C, N).

negNormForm(all(R,C), all(R,N)):-
	!,negNormForm(C, N).
	
negNormForm(atleast(N,R,C), atleast(N,R,C2)):-
	!,negNormForm(C, C2).

negNormForm(atmost(N,R,C), atmost(N,R,C2)):-
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
def(all(R,C),Prefix,[all(R,Q)|Defs]):-
	def2(C,Prefix,1,Q,Defs).
def(some(R,C),Prefix,[some(R,Q)|Defs]):-
	def2(C,Prefix,1,Q,Defs).
def(atleast(N,R,C),Prefix,[atleast(N,R,Q)|Defs]):-
	def2(C,Prefix,1,Q,Defs).
def(atmost(N,R,C),Prefix,[atmost(N,R,Q)|Defs]):-
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
	  select(C,L,Rest),
	  def2(C,Prefix,1,Q,Defs1),
	  append(Defs1,Defs2,Defs),	  
	  def(or([Q|Rest]),Prefix,Defs2)
	; Cs = [C],
	  select(C,L,LRest),
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
def2(all(R,C),Prefix,Pol,Q1,[D|Defs]):-
	def2(C,Prefix,Pol,Q2,Defs),
	def2(all(R,Q2),Prefix,Pol,Q1,[D]).
def2(some(R,C),Prefix,Pol,Q1,[D|Defs]):-
	def2(C,Prefix,Pol,Q2,Defs),
	def2(some(R,Q2),Prefix,Pol,Q1,[D]).
def2(atleast(R,C),Prefix,Pol,Q1,[D|Defs]):-
	def2(C,Prefix,Pol,Q2,Defs),
	def2(atleast(R,Q2),Prefix,Pol,Q1,[D]).
def2(atmost(R,C),Prefix,Pol,Q1,[D|Defs]):-
	Pol1 is Pol * -1,
	def2(C,Prefix,Pol1,Q2,Defs),
	def2(atmost(R,Q2),Prefix,Pol,Q1,[D]).
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
	  Cs = [C,_|_] ->
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
not_embedded(all(_,C)):-
	literal_concept(C), !.
not_embedded(some(_,C)):-
	literal_concept(C), !.
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
defName(atleast(Num,R,C),N):-
	number_codes(Num,NumCode), atom_codes(Num1,NumCode),
	atom_concat(atleast_,Num1,N1),
	atom_concat(N1,'_',N2),
	defName(R,N3),
	atom_concat(N2,N3,N4),
	atom_concat(N4,'_',N5),
	defName(C,N6),
	atom_concat(N5,N6,N).
defName(atmost(Num,R,C),N):-
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

/***********************************************************/
/* Regen ez vegezte a strukturalis trafot, de van mar jobb */
/***********************************************************/
/*
				% defNormForms(+Cs,+Name,-DCs): DCs a Cs listaban talalhato
% fogalmak egyertelmuen definialt formaibol allo lista es az uj
% fogalomneveket Name atomtol kezdodoen vesszuk
defNormForms([],_,[]).
defNormForms([C|Cs],Name,Defs):-
	def_old(C,Name,Defs,DEnd,NewName),defNormForms(Cs,NewName,DEnd).

% def(+C, +Name, -Defs, ?DEnd, -NewName):
% C egy NNF ALCHIQ fogalom
% Name az az elso nev, amit Def(C)-ben uj fogalomnevkent felhasznalhatunk
% Defs a Def(C)-be tartozo fogalmakkal kezdodo, DEnd vegu lista
% NewName a kovetkezo szabad "uj fogalomnev"	
def_old(C,Name,[C|X],X,Name):-
	\+ literal_concept(C),
	rposition(C,Name,_,CQp),
	\+ rposition(CQp,_,_,_), !.	
def_old(C,Name,Defs,DEnd,NewName):-
	\+literal_concept(C),
	rposition(C,Name,NQCp,CQp),!,
	Defs=[NQCp|Ds],
	nextName(Name,NN),
	def_old(CQp,NN,Ds,DEnd,NewName).
def_old(C,Name,[C|X],X,Name).

% rposition(+C,+Q,-NQCp,-CQp):
% C fogalomban letezik egy kicserelesre alkalmas p pozicio
% ha Cp a p pozicio alatt talalhato reszfogalom, akkor
% NQCp = or([not(nconcept(Q)),Cp])  ha pol(C,p) = 1
% NQCp = or([nconcept(Q),not(Cp)])  ha pol(C,p) = -1
% CQp pedig az a fogalom, melyet C p-beli reszfogalmanak
% Q-ra cserelesevel kapunk
rposition(all(R,L),Q,NQCp,all(R,X)):-
	\+literal_concept(L),reposition(L,Q,1,NQCp,X).

rposition(some(R,L),Q,NQCp,some(R,X)):-
	\+literal_concept(L),reposition(L,Q,1,NQCp,X).

rposition(and(L),Q,NCQp,and(Ls)):-
	not_literal(L,NL,Ls,X),reposition(NL,Q,1,NCQp,X). % L=[L1,L2,...,NL,...],Ls=[L1,L2,...,X,...]

rposition(or(L),Q,NCQp,or(Ls)):-
	not_literal(L,NL,Ls,X),reposition(NL,Q,1,NCQp,X).
	
rposition(atleast(N,R,L),Q,NQCp,atleast(N,R,X)):-
	\+literal_concept(L),reposition(L,Q,1,NQCp,X).
	
rposition(atmost(N,R,L),Q,NQCp,atmost(N,R,X)):-
	\+literal_concept(L),reposition(L,Q,-1,NQCp,X).

% reposition(+C,+Q,+Pol,-NQCp,-CQp):
% ugyanaz, mint rposition/4 de megengedjuk az ures poziciot is
% illetve nyomon kovetjuk a vizsgalt pozicio polaritasat
reposition(all(R,L),Q,1,NQCp,CQp):-
	(	literal_concept(L) ->	NQCp=or([not(nconcept(Q)),all(R,L)]),CQp=nconcept(Q);		
	  CQp=all(R,X),reposition(L,Q,1,NQCp,X)).
	  
reposition(all(R,L),Q,-1,NQCp,CQp):-
	( literal_concept(L) ->
	    negNormForm(not(all(R,L)),Negated),
	    NQCp=or([nconcept(Q),Negated]),CQp=nconcept(Q);		
	    CQp=all(R,X),reposition(L,Q,-1,NQCp,X)
	).

reposition(some(R,L),Q,1,NQCp,CQp):-
	(	literal_concept(L) ->	NQCp=or([not(nconcept(Q)),some(R,L)]),CQp=nconcept(Q);
	    CQp=some(R,X),reposition(L,Q,1,NQCp,X)).

reposition(some(R,L),Q,-1,NQCp,CQp):-
	(	literal_concept(L) ->	negNormForm(not(some(R,L)),Negated),
	    NQCp=or([nconcept(Q),Negated]),CQp=nconcept(Q);
	    CQp=some(R,X),reposition(L,Q,1,NQCp,X)).

reposition(and(L),Q,1,NCQp,CQp):-
	(	not_literal(L,NL,Ls,X)	->	CQp=and(Ls),reposition(NL,Q,1,NCQp,X); % L=[L1,L2,...,NL,...],Ls=[L1,L2,...,X,...]
	    NCQp=or([not(nconcept(Q)),and(L)]),CQp=nconcept(Q)).

reposition(and(L),Q,-1,NCQp,CQp):-
	(	not_literal(L,NL,Ls,X)	->	CQp=and(Ls),reposition(NL,Q,-1,NCQp,X); % L=[L1,L2,...,NL,...],Ls=[L1,L2,...,X,...]
	    negNormForm(not(and(L)),Negated),
	    NCQp=or([nconcept(Q),Negated]),CQp=nconcept(Q)).

reposition(or(L),Q,1,NCQp,CQp):-
	(	not_literal(L,NL,Ls,X)	->	CQp=or(Ls),reposition(NL,Q,1,NCQp,X);
	  NCQp=or([not(nconcept(Q))|L]),CQp=nconcept(Q)).
	  
reposition(or(L),Q,-1,NCQp,CQp):-
	(	not_literal(L,NL,Ls,X)	->	CQp=or(Ls),reposition(NL,Q,-1,NCQp,X);
		negNormForm(not(or(L)),Negated),
	  NCQp=or([nconcept(Q),Negated]),CQp=nconcept(Q)).
	  
reposition(atleast(N,R,L),Q,1,NQCp,CQp):-
	(	literal_concept(L) ->	NQCp=or([not(nconcept(Q)),atleast(N,R,L)]),CQp=nconcept(Q);		
	  CQp=atleast(N,R,X),reposition(L,Q,1,NQCp,X)).
	  
reposition(atleast(N,R,L),Q,-1,NQCp,CQp):-
	(	literal_concept(L) ->	negNormForm(not(atleast(N,R,L)),Negated),
													NQCp=or([nconcept(Q),Negated]),CQp=nconcept(Q);		
	  CQp=atleast(N,R,X),reposition(L,Q,-1,NQCp,X)).
	  
reposition(atmost(N,R,L),Q,1,NQCp,CQp):-
	(	literal_concept(L) ->	NQCp=or([not(nconcept(Q)),atmost(N,R,L)]),CQp=nconcept(Q);		
	  CQp=atmost(N,R,X),reposition(L,Q,-1,NQCp,X)).
	  
reposition(atmost(N,R,L),Q,-1,NQCp,CQp):-
	(	literal_concept(L) ->	negNormForm(not(atmost(N,R,L)),Negated),
													NQCp=or([nconcept(Q),Negated]),CQp=nconcept(Q);		
	  CQp=atmost(N,R,X),reposition(L,Q,1,NQCp,X)).	  	  
*/
/*
% not_literal(+L,-NL,-Ls,-X): L fogalmak listaja,
% NL az elso nem literal fogalom a listaban
% Ls-t ugy kapjuk, hogy L-ben NL-t X-re csereljuk
not_literal([L|Lt],NL,[L|Ls],X):-
	literal_concept(L),!,not_literal(Lt,NL,Ls,X).
not_literal([L|Lt],L,[X|Lt],X). % \+literal_concept(L)
*/

% nextName(+Name,-NextName): NextName a lexikografikus rendezes
% szerinti Name utani betusorozat
nextName(N,NN):-
	atom_codes(N,Codes),
	reverse(Codes,R),
	nextList(R,NR),reverse(NR,NC),
	atom_codes(NN,NC).

nextList([C|Cs],[NC|NCs]):-
	(C < 122 ->
	    NC is C+1, NCs=Cs;
	    NC=97,nextList(Cs,NCs)).
nextList([],[97]).




/*****************************************************/
/**** Elsorendu logikai formulakra valo lekepezes ****/
/*****************************************************/
% toClauseList(+DLList,-ClauseList): ClauseList az X eleme DL allitasoknak
% megfelelo elsorendu logikai klozok listaja, ahol DL DLList-beli
% elem
toClauseList(DLList,ClauseList):-
	toClauseList(DLList,_,ClauseList,f,_).

toClauseList([],_,[],FUN,FUN).
toClauseList([inv(arole(R),arole(S))|Ds],X,ClauseList,FUN,NFUN):- !,
	ClauseList = [[1,[not(arole(R,X,Y)),arole(S,Y,X)]],[1,[not(arole(S,X,Y)),arole(R,Y,X)]]|Rest],
	toClauseList(Ds,X,Rest,FUN,NFUN).
toClauseList([or(L)|Ds],X,Fs,FUN,NFUN):-
	select(and(A),L,LRest), !,
	toLiteralList(LRest,X,Literals),
	toLiteralList(A,X,AndLiterals),
	combineWithAnd(AndLiterals,Literals,Clauses),
	append(Clauses,FRest,Fs),
	toClauseList(Ds,X,FRest,FUN,NFUN).
toClauseList([and(L)|Ds],X,Fs,FUN,NFUN):- !,
	toLiteralList(L,X,Literals),
	combineWithAnd(Literals,[],Clauses),
	append(Clauses,FRest,Fs),
	toClauseList(Ds,X,FRest,FUN,NFUN).
toClauseList([D|Ds],X,[F|Fs],FUN,NFUN):-
	toClause(D,X,F,FUN,FUN1),toClauseList(Ds,X,Fs,FUN1,NFUN).

% toLiteralList(+Ds,+X,-Ls):- Ls literalok listaja mely elemei az
% X eleme D DL allitasnak felelnek meg, ahol D eleme Ds-nek
toLiteralList([],_,[]).
toLiteralList([D|Ds],X,[L|Ls]):-
	(
	  D = aconcept(C) -> L = aconcept(C,X)
	; D = nconcept(C) -> L = nconcept(C,X)
	; D = not(aconcept(C)) -> L = not(aconcept(C,X))
	; D = not(nconcept(C)) -> L = not(nconcept(C,X))
	),
	toLiteralList(Ds,X,Ls).

% combineWithAnd(+As,+Ls,-Clauses):- az or([and(As)|Ls])
% formulanak megfelelo klozlista Clauses
combineWithAnd(Ands,Literals,Clauses):-
	findall([5,Clause], (
			     member(A,Ands),
			     Clause = [A|Literals]
			   ), Clauses
	      ).
	 


% toClause(+DLExpr,X,-Clause,+funName,-newFunName):
% Clause az X eleme DLExpr kifejezes elsorendu
% logikai megfeleloje kloz formaban, ahol a fuggvenyszimbolumok
% funName es newFunName koze esnek
toClause(subrole(arole(R),arole(S)),X,[1,[not(arole(R,X,Y)),arole(S,X,Y)]],FUN,FUN):- !.
toClause(or(L),X,Clause,FUN,NFUN):- !,
	(
	  select(Concept,L,LRest),
	  \+ literal_concept(Concept) ->
	  toFOL(Concept,X,FOLConcept,Type,FUN,NFUN),
	  toLiteralList(LRest,X,Literals),
	  append(FOLConcept,Literals,AllLiterals),
	  Clause = [Type,AllLiterals]
	;
	  toLiteralList(L,X,Literals),
	  NFUN = FUN,
	  Clause = [5,Literals]
	).
toClause(C,X,[Type,C2],FUN,NFUN):-
	toFOL(C,X,C2,Type,FUN,NFUN).

toFOL(top,_,[true],5,FUN,FUN):- !.
toFOL(bottom,_,[not(true)],5,FUN,FUN):- !.
toFOL(not(top),_,[not(true)],5,FUN,FUN):- !.
toFOL(not(bottom),_,[true],5,FUN,FUN):- !.
toFOL(aconcept(C),X,[aconcept(C,X)],5,FUN,FUN).
toFOL(nconcept(C),X,[nconcept(C,X)],5,FUN,FUN).
toFOL(not(aconcept(C)),X,[not(aconcept(C,X))],5,FUN,FUN).
toFOL(not(nconcept(C)),X,[not(nconcept(C,X))],5,FUN,FUN).
toFOL(not(not(aconcept(C))),X,[aconcept(C,X)],5,FUN,FUN).
toFOL(not(not(nconcept(C))),X,[nconcept(C,X)],5,FUN,FUN).

toFOL(all(arole(R),C),X,[not(arole(R,X,Y))|FOLC],7,FUN,NFUN):-
	toFOL(C,Y,FOLC,_,FUN,NFUN).
toFOL(some(arole(R),C),X,[atleast(1,arole(R,X,fun(FUN,X)),FOLC)],3,FUN,NFUN):-
	nextName(FUN,F1),toFOL(C,fun(FUN,X),[FOLC],_,F1,NFUN).
toFOL(atleast(N,arole(R),C),X,[atleast(N,arole(R,X,fun(FUN,X)),FOLC)],3,FUN,NFUN):-
	nextName(FUN,F1),toFOL(C,fun(FUN,X),[FOLC],_,F1,NFUN).
toFOL(atmost(N,arole(R),C),X,Literals,7,FUN,NFUN):-
	N1 is N + 1,
	createVars(N1,Vars),
	atmostLiterals(Vars,arole(R),C,X,Literals,FUN,NFUN).


% createVars(+N,-Vars):-
createVars(0,[]):- !.
createVars(N,[_|Rest]):-	
	N > 0,
	N1 is N - 1,	
	createVars(N1,Rest).
	
	
atmostLiterals([],_,_,_,[],FUN,FUN).	
atmostLiterals([V|Vars],arole(R),C,X,[not(arole(R,X,V)),FOLC|Literals],FUN,NFUN):-
	equalLiterals(V,Vars,Equals),
	append(Equals,Rest,Literals),
	toFOL(not(C),V,[FOLC],_,FUN,NFUN1),
	atmostLiterals(Vars,arole(R),C,X,Rest,NFUN1,NFUN).

equalLiterals(_,[],[]).
equalLiterals(V,[V2|Vars],[eq(V,V2)|Equals]):-
	equalLiterals(V,Vars,Equals).

/*****************************************************************/
/******************** Klozformara hozas **************************/
/*****************************************************************/

% cls(+F,-Cs): Cs az F elsorendu logikai formulanak megfelelo
% klozokbol allo lista
% egy kloz formatuma: [tipus, literalok listaja]
cls(and(L),Cs):-
	!, delete(L,true,L2),
	(
	  member(not(true),L2) -> Cs=[[]]
	; and_cls(L2,Cs)
	).
cls(or(L),Cs):-
	!, delete(L,not(true),L2),
	(
	  member(true,L2) -> Cs = []
	; or_cls(L2,Cs)
	).
cls(L,[[L]]).
	
and_cls([],[]).
and_cls([L|Ls],CList):-
	cls(L,C),
	append(C,C2,CList),
	and_cls(Ls,C2).

% or_cls(+Ors,-LClss): LClss az or(Ors) formulanak megfelelo
% klozokbol allo lista
or_cls(Ors,LClss):-
	or_cls(Ors,[[]],LClss).

% or_cls(+Ors,+LClss0,-LCls): elozo gyujtoargumentummal
or_cls([],LClss,LClss).
or_cls([or(L)|Ls],LClss0,LClss):-
	!,
	or_cls(L,LClss0,LClss1),or_cls(Ls,LClss1,LClss).
or_cls([L|Ls],LClss0,LClss):-
	cls(L,L2),
	combine_to_lists(L2,LClss0,LClss1),
	or_cls(Ls,LClss1,LClss).
	

% combine_to_lists(List1,List2,ResList): ResList-et ugy kapjuk, hogy
% minden List2-ben talalhato listahoz List1 1-1 elemlistajat hozzafuzzuk
% az osszes lehetseges modon

combine_to_lists(As,L,Res):-
	combine_to_lists(As,L,[],Res).
combine_to_lists([A|As],L,R0,R):-
	combine_to(A,L,R1),append(R1,R0,R2),combine_to_lists(As,L,R2,R).
combine_to_lists([],_,R,R).

% combine_to(+A,+L,-R): Az L listaban talalhato valamennyi listahoz
% A listat hozzafuzve kapjuk az R listat
combine_to(_,[],[]).
combine_to(A,[L|Ls],[R|Rs]):-
	append(A,L,R),
	combine_to(A,Ls,Rs).

% list_cls(+Fs, -Clss): Fs elsorendu formulak listaja
% Clss az Fs-beli formulaknak megfelelo klozok listaja
list_cls([],[]).
list_cls([F|Fs],Clss):-
	cls(F,Clss1),
	append(Clss1,Clss2,Clss),list_cls(Fs,Clss2).


/**************************** klozok tipusokba sorolasa ***************/

clause_types([C|Cs],[TC|TCs]):-
	clause_type(C,TC),
	clause_types(Cs,TCs).
clause_types([],[]).

% clause_type(+C,[-T,+C]):- a C kloz tipusa T
clause_type(C,[T,C]):-
	contains_struct(C,arole(_,_,_)), !,
	(
	  contains_struct(C,fun(_,_)) -> T = 3
	; contains_struct(C,eq(_,_)) -> T = 7
	; contains_struct(C,aconcept(_,_)) -> T = 7
	; contains_struct(C,nconcept(_,_)) -> T = 7
	; T = 1
	).
clause_type(C,[5,C]):-
	contains_struct(C,fun(_,_)), !.
clause_type(C,[50,C]).