:- module(dl_to_fol, [axiomsToNNFConcepts/2, defNormForms/2, toFOLList/3, list_cls/2, negNormForm/2]).

:- use_module(library(lists)).

/***************************************************/
/********* Negacios normalformara hozas ************/
/***************************************************/

% axiomsToNNFConcepts(+Axioms, -Concepts): Concepts az Axioms
% listaban levo axiomak belsositesei negacios normalformainak listaja
% Axioms csak szereptartalmazasi axiomakbol all
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
negNormForm(not(aconcept(X)),not(aconcept(X))).
negNormForm(not(top),bottom).
negNormForm(not(bottom),top).

negNormForm(not(not(X)),N):-
	   !,negNormForm(X,N).

negNormForm(not(and(Xs)),or(Ns)):-
	!,nnegForms(Xs,Ns).

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

negNormForm(and(Xs), and(Ns)):-
	!, negForms(Xs,Ns2),
	simplifyAnd(Ns2,Ns).	

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
	(	select(and(Zs),Xs,Xs2)	->	append(Zs,Xs2,Xs3),
																simplifyAnd(Xs3,Ys)
	;	Ys = Xs
	).
	
%simplifyOr(+Xs,-Ys)
simplifyOr(Xs,Ys):-	
	(	select(or(Zs),Xs,Xs2)	->		append(Zs,Xs2,Xs3),
																simplifyOr(Xs3,Ys)
	;	Ys = Xs
	).	
	
/*******************************************************/
/************* Strukturalis transzformacio  ************/
/*******************************************************/

% defNormForms(+Cs,-DCs): DCs a Cs listaban talalhato
% fogalmak egyertelmuen definialt formaibol allo lista
defNormForms(Cs,DCs):-
	defNormForms(Cs,newaaa,DCs).

% defNormForms(+Cs,+Name,-DCs): DCs a Cs listaban talalhato
% fogalmak egyertelmuen definialt formaibol allo lista es az uj
% fogalomneveket Name atomtol kezdodoen vesszuk
defNormForms([],_,[]).
defNormForms([C|Cs],Name,Defs):-
	def(C,Name,Defs,DEnd,NewName),defNormForms(Cs,NewName,DEnd).

% def(+C, +Name, -Defs, ?DEnd, -NewName):
% C egy NNF ALCHIQ fogalom
% Name az az elso nev, amit Def(C)-ben uj fogalomnevkent felhasznalhatunk
% Defs a Def(C)-be tartozo fogalmakkal kezdodo, DEnd vegu lista
% NewName a kovetkezo szabad "uj fogalomnev"	
def(C,Name,[C|X],X,Name):-
	\+ literal_concept(C),
	rposition(C,Name,_,CQp),
	\+ rposition(CQp,_,_,_), !.	
def(C,Name,Defs,DEnd,NewName):-
	\+literal_concept(C),
	rposition(C,Name,NQCp,CQp),!,
	Defs=[NQCp|Ds],
	nextName(Name,NN),
	def(CQp,NN,Ds,DEnd,NewName).
def(C,Name,[C|X],X,Name).


% literal_concept(+C): igaz, ha C literal fogalom
literal_concept(C):-
	C=aconcept(X);C=not(X);C=top;C=bottom;C=nconcept(X).

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
	(	literal_concept(L) ->	negNormForm(not(all(R,L)),Negated),
													NQCp=or([nconcept(Q),Negated]),CQp=nconcept(Q);		
	  CQp=all(R,X),reposition(L,Q,-1,NQCp,X)).

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

% not_literal(+L,-NL,-Ls,-X): L fogalmak listaja,
% NL az elso nem literal fogalom a listaban
% Ls-t ugy kapjuk, hogy L-ben NL-t X-re csereljuk
not_literal([L|Lt],NL,[L|Ls],X):-
	literal_concept(L),!,not_literal(Lt,NL,Ls,X).
not_literal([L|Lt],L,[X|Lt],X). % \+literal_concept(L)


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

% toFOL(+DLExpr,X,-FOLExpr,+funName,-newFunName):
% FOLExpr az X eleme DLExpr kifejezes elsorendu
% logikai megfeleloje, ahol a fuggvenyszimbolumok
% funName es newFunName koze esnek
toFOL(top,_,true,FUN,FUN).
toFOL(bottom,_,not(true),FUN,FUN).
toFOL(not(C),X,not(F),FUN,NFUN):-
	toFOL(C,X,F,FUN,NFUN).
toFOL(aconcept(C),X,aconcept(C,X),FUN,FUN).
toFOL(nconcept(C),X,nconcept(C,X),FUN,FUN).
toFOL(all(arole(R),C),X,or([not(arole(R,X,Y)),FOLC]),FUN,NFUN):-
	toFOL(C,Y,FOLC,FUN,NFUN).
toFOL(some(arole(R),C),X,and([arole(R,X,fun(FUN,X)),FOLC]),FUN,NFUN):-
	nextName(FUN,F1),toFOL(C,fun(FUN,X),FOLC,F1,NFUN).
toFOL(or(L),X,or(FL),FUN,NFUN):-
	toFOLList(L,X,FL,FUN,NFUN).
toFOL(and(L),X,and(FL),FUN,NFUN):-
	toFOLList(L,X,FL,FUN,NFUN).

toFOL(atleast(N,arole(R),C),X,and(Literals),FUN,NFUN):-	
	createSkolems(N,X,Skolems,FUN,NFUN1),	
	atleastLiterals(Skolems,arole(R),C,X,Literals,NFUN1,NFUN).

toFOL(atmost(N,arole(R),C),X,or(Literals),FUN,NFUN):-
	N1 is N + 1,
	createVars(N1,Vars),
	atmostLiterals(Vars,arole(R),C,X,Literals,FUN,NFUN).
	
toFOL(subrole(arole(R),arole(S)),X,or([not(arole(R,X,Y)),arole(S,X,Y)]),FUN,FUN).
toFOL(inv(arole(R),arole(S)),X,and([or([not(arole(R,X,Y)),arole(S,Y,X)]),or([not(arole(S,X,Y)),arole(R,Y,X)])]),FUN,FUN).

% toFOLList(+DLList,X,-FOLList): FOLList az X eleme DL allitasoknak
% megfelelo elsorendu logikai formulak listaja, ahol DL DLList-beli
% elem
toFOLList(DLList,X,FOLList):-
	toFOLList(DLList,X,FOLList,f,_).
toFOLList([],_,[],FUN,FUN).
toFOLList([inv(arole(R),arole(S))|Ds],X,FOLList,FUN,NFUN):-
	!, FOLList = [or([not(arole(R,X,Y)),arole(S,Y,X)]),or([not(arole(S,X,Y)),arole(R,Y,X)])|Rest],
	toFOLList(Ds,X,Rest,FUN,NFUN).
	
toFOLList([D|Ds],X,[F|Fs],FUN,NFUN):-
	toFOL(D,X,F,FUN,F1),toFOLList(Ds,X,Fs,F1,NFUN).

%	createSkolems(+N,+X,-Vars,FUN,NFUN):-
createSkolems(0,_,[],FUN,FUN):- !.
createSkolems(N,X,[fun(FUN,X)|Rest],FUN,NFUN):-	
	N > 0,
	N1 is N - 1,
	nextName(FUN,F1),
	createSkolems(N1,X,Rest,F1,NFUN).

%	createVars(+N,-Vars):-
createVars(0,[]):- !.
createVars(N,[_|Rest]):-	
	N > 0,
	N1 is N - 1,	
	createVars(N1,Rest).
	
atleastLiterals([],_,_,_,[],FUN,FUN).
atleastLiterals([S|Skolems],arole(R),C,X,Literals,FUN,NFUN):-
	Literals = [arole(R,X,S),FOLC|Rest],
	toFOL(C,S,FOLC,FUN,NFUN1),
	notEqualLiterals(S,Skolems,NotEquals),
	append(NotEquals,Rest2,Rest),
	atleastLiterals(Skolems,arole(R),C,X,Rest2,NFUN1,NFUN).
	
	
atmostLiterals([],_,_,_,[],FUN,FUN).	
atmostLiterals([V|Vars],arole(R),C,X,[not(arole(R,X,V)),FOLC|Literals],FUN,NFUN):-
	equalLiterals(V,Vars,Equals),
	append(Equals,Rest,Literals),
	toFOL(not(C),V,FOLC,FUN,NFUN1),	
	atmostLiterals(Vars,arole(R),C,X,Rest,NFUN1,NFUN).

equalLiterals(_,[],[]).
equalLiterals(V,[V2|Vars],[eq(V,V2)|Equals]):-
	equalLiterals(V,Vars,Equals).
	
notEqualLiterals(_,[],[]).
notEqualLiterals(S,[S2|Skolems],[not(eq(S,S2))|NotEquals]):-
	notEqualLiterals(S,Skolems,NotEquals).


/*****************************************************************/
/******************** Klozformara hozas **************************/
/*****************************************************************/

% cls(+F,-Cs): Cs az F elsorendu logikai formulanak megfelelo
% klozokbol allo lista
cls(and(L),Cs):-		
	!, delete(L,true,L2),
	(	member(not(true),L2) ->	Cs=[[]]
	;													and_cls(L2,Cs)
	).
cls(or(L),Cs):-
	!, delete(L,not(true),L2),
	( member(true,L2)	->	Cs = []
	;											or_cls(L2,Cs)
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
	