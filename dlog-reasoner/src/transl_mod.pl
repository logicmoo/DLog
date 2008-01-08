/***   T-doboz axiomak fuggvenyjelmentes klozokra valo lekepezese

    Entry point:
      - axioms_to_clauses(+Axioms,-RClauses)
      
  Keszitette: Borosan Peter (BME-SZIT)
              pborosan@cs.bme.hu
            
***/
:- module(transl_mod,[axioms_to_clauses/2]).

:- use_module(library(lists),[append/3,select/3,member/2,delete/3,suffix/2,reverse/2]).
:- use_module(library(terms),[subsumes/2,term_variables/2]).
:- use_module(library(ordsets),[list_to_ord_set/2,ord_subset/2]).


/***************************************************/
/********* Negacios normalformara hozas ************/
/***************************************************/

% axiomsToNNFConcepts(+Axioms, -Concepts): Concepts az Axioms
% listaban levo axiomak belsositesei negacios normalformainak listaja
axiomsToNNFConcepts([implies(C1, C2)|As], [or([N1, N2])|Cs]):-
	negNormForm(not(C1),N1), negNormForm(C2, N2),
	axiomsToNNFConcepts(As, Cs).
axiomsToNNFConcepts([equiv(C1, C2)|As], [or([NN1, N2]), or([NN2, N1])|Cs]):-
	negNormForm(C1,N1), negNormForm(C2,N2),
	negNormForm(not(C1), NN1), negNormForm(not(C2),NN2),
	axiomsToNNFConcepts(As, Cs).
axiomsToNNFConcepts([],[]).

% negNormForm(+Concept, -NegForm): NegForm Concept fogalom negacios
% normalformaju megfeleloje
negNormForm(top, top).
negNormForm(bottom, bottom).
negNormForm(aconcept(X), aconcept(X)).
negNormForm(not(aconcept(X)),not(aconcept(X))).

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

negNormForm(and(Xs), and(Ns)):-
	!,negForms(Xs, Ns).

negNormForm(or(Xs), or(Ns)):-
	!,negForms(Xs, Ns).

negNormForm(some(R,C), some(R,N)):-
	!,negNormForm(C, N).

negNormForm(all(R,C), all(R,N)):-
	!,negNormForm(C, N).


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


/*******************************************************/
/************* Strukturalis transzformacio  ************/
/*******************************************************/

% defNormForms(+Cs,-DCs): DCs a Cs listaban talalhato
% fogalmak egyertelmuen definialt formaibol allo lista
defNormForms(Cs,DCs):-
	defNormForms(Cs,q,DCs).

% defNormForms(+Cs,+Name,-DCs): DCs a Cs listaban talalhato
% fogalmak egyertelmuen definialt formaibol allo lista es az uj
% fogalomneveket Name atomtol kezdodoen vesszuk
defNormForms([],_,[]).
defNormForms([C|Cs],Name,Defs):-
	def(C,Name,Defs,DEnd,NN),defNormForms(Cs,NN,DEnd).

% def(+C, +Name, -Defs, ?DEnd, -NewName):
% C egy NNF ALC fogalom
% Name az az elso nev, amit Def(C)-ben uj fogalomnevkent felhasznalhatunk
% Defs a Def(C)-be tartozo fogalmakkal kezdodo, DEnd vegu lista
% NewName a kovetkezo szabad "uj fogalomnev"
def(C,Name,Defs,DEnd,NewName):-
	\+literal_concept(C),
	rposition(C,Name,NQCp,CQp),!,Defs=[NQCp|Ds],
	nextName(Name,NN),def(CQp,NN,Ds,DEnd,NewName).
def(C,Name,[C|X],X,Name).


% literal_concept(+C): igaz, ha C literal fogalom
literal_concept(C):-
	C=aconcept(X);C=not(X);C=top;C=bottom;C=nconcept(X).

% rposition(+C,+Q,-NQCp,-CQp):
% C fogalomban letezik egy kicserelesre alkalmas p pozicio
% ha Cp a p pozicio alatt talalhato reszfogalom, akkor
% NQCp = or([not(nconcept(Q)),Cp])
% CQp pedig az a fogalom, melyet C p-beli reszfogalmanak
% Q-ra cserelesevel kapunk
rposition(all(R,L),Q,NQCp,all(R,X)):-
	\+literal_concept(L),reposition(L,Q,NQCp,X).

rposition(some(R,L),Q,NQCp,some(R,X)):-
	\+literal_concept(L),reposition(L,Q,NQCp,X).

rposition(and(L),Q,NCQp,and(Ls)):-
	not_literal(L,NL,Ls,X),reposition(NL,Q,NCQp,X). % L=[L1,L2,...,NL,...],Ls=[L1,L2,...,X,...]

rposition(or(L),Q,NCQp,or(Ls)):-
	not_literal(L,NL,Ls,X),reposition(NL,Q,NCQp,X).

% reposition(+C,+Q,-NQCp,-CQp):
% ugyanaz, mint rposition/4 de megengedjuk az ures poziciot is
reposition(all(R,L),Q,NQCp,CQp):-
	(literal_concept(L)->NQCp=or([not(nconcept(Q)),all(R,L)]),CQp=nconcept(Q);
	    CQp=all(R,X),reposition(L,Q,NQCp,X)).

reposition(some(R,L),Q,NQCp,CQp):-
	(literal_concept(L)->NQCp=or([not(nconcept(Q)),some(R,L)]),CQp=nconcept(Q);
	    CQp=some(R,X),reposition(L,Q,NQCp,X)).

reposition(and(L),Q,NCQp,CQp):-
	(not_literal(L,NL,Ls,X)->CQp=and(Ls),reposition(NL,Q,NCQp,X); % L=[L1,L2,...,NL,...],Ls=[L1,L2,...,X,...]
	    NCQp=or([not(nconcept(Q)),and(L)]),CQp=nconcept(Q)).

reposition(or(L),Q,NCQp,CQp):-
	(not_literal(L,NL,Ls,X)->CQp=or(Ls),reposition(NL,Q,NCQp,X);
	    NCQp=or([not(nconcept(Q))|L]),CQp=nconcept(Q)).

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

% toFOLList(+DLList,X,-FOLList): FOLList az X eleme DL allitasoknak
% megfelelo elsorendu logikai formulak listaja, ahol DL DLList-beli
% elem
toFOLList(DLList,X,FOLList):-
	toFOLList(DLList,X,FOLList,f,_).
toFOLList([],_,[],FUN,FUN).
toFOLList([D|Ds],X,[F|Fs],FUN,NFUN):-
	toFOL(D,X,F,FUN,F1),toFOLList(Ds,X,Fs,F1,NFUN).


/*****************************************************************/
/******************** Klozformara hozas **************************/
/*****************************************************************/

% cls(+F,-Cs): Cs az F elsorendu logikai formulanak megfelelo
% klozokbol allo lista
cls(and(L),LT):-
	!,\+member(not(true)),delete(L,true,LT).
cls(or(L),Cs):-
	!,clss(L,Cs).

% clss(+Ors,-LClss): LClss az or(Ors) formulanak megfelelo
% klozokbol allo lista
clss(Ors,LClss):-
	clss(Ors,[[]],LClss).

% clss(+Ors,+LClss0,-LCls): elozo gyujtoargumentummal
clss([or(L)|Ls],LClss0,LClss):-
	!,clss(L,LClss0,LClss1),clss(Ls,LClss1,LClss).
clss([and(L)|Ls],LClss0,LClss):-
	!,(member(not(true),L) ->
	   clss(Ls,LClss0,LClss);
	   delete(L,true,LT),combine_to_lists(LT,LClss0,LClss1),clss(Ls,LClss1,LClss)).
clss([],LClss,LClss).
clss([L|Ls],LClss0,LClss):-
	(L=true ->
	    LClss=[];
	    (L=not(true) ->
		clss(Ls,LClss0,LClss);
		combine_to(L,LClss0,LClss1),clss(Ls,LClss1,LClss))).

% combine_to_lists(List1,List2,ResList): ResList-et ugy kapjuk, hogy
% minden List2-ben talalhato listahoz List1 1-1 elemet az osszes
% lehetseges modon hozzavesszuk
combine_to_lists(As,L,Res):-
	combine_to_lists(As,L,[],Res).
combine_to_lists([A|As],L,R0,R):-
	combine_to(A,L,R1),append(R1,R0,R2),combine_to_lists(As,L,R2,R).
combine_to_lists([],_,R,R).

% combine_to(+A,+L,-R): Az L listaban talalhato valamennyi listahoz
% A-t hozzafuzve kapjuk az R listat
combine_to(_,[],[]).
combine_to(A,[L|Ls],[[A|L]|Rs]):-
	combine_to(A,Ls,Rs).

% list_cls(+Fs, -Clss): Fs elsorendu formulak listaja
% Clss az Fs-beli formulaknak megfelelo klozok listaja
list_cls([],[]).
list_cls([F|Fs],Clss):-
	cls(F,Clss1),append(Clss1,Clss2,Clss),list_cls(Fs,Clss2).



/******************************************************************/
/************ Rendezett rezolucios kovetkeztetes ******************/
/******************************************************************/


/************ Rezolucioban resztvevo literal kijelolese *****************/

% cls_to_ocls(+Cls,-[RL|TL]): Cls klozban RL literal vehet reszt
% rezolucioban (kivalasztott vagy maximalis), TL a tobbi literal
cls_to_ocls([L|Ls],[RL|TL]):-
	not_true([L|Ls]),
	(role_literal(L) ->
	    RL=L,TL=Ls;
	    res_lit([L|Ls],RL,TL)).

% role_literal(+L): igaz, ha L egy ketargumentumu literal
role_literal(arole(_,_,_)):-!.
role_literal(not(arole(_,_,_))):-!.

% res_lit(+Clause,-R,-Ls): A Clause klozbol R literal vehet reszt
% rezolucioban, Ls a tobbi literal
res_lit([L],L,[]):-!.
res_lit([L1,L2|Ls],M,TL):-
	(role_literal(L2) ->
	    M=L2,TL=[L1|Ls];
	    (greater(L1,L2) ->
		M0=L1,TL=[L2|TL2];
		M0=L2,TL=[L1|TL2]),
	    res_lit([M0|Ls],M,TL2)).

% concept_name(+L,-N,-T): Az L egyargumentumu literal a C fogalomra es
% a T termre vonatkozik, N-et ugy kapjuk, hogy C ele a-t vagy n-t irunk
% az alapjan hogy regi vagy uj valtozorol van e szo
concept_name_arg(aconcept(C,T),N,T):-
	atom_concat(a,C,N).
concept_name_arg(nconcept(C,T),N,T):-
	atom_concat(n,C,N).
concept_name_arg(not(A),N,T):-
	concept_name_arg(A,N,T).

% greater(A,B): A es B egyargumentumu literal es A>B
greater(A,B):-
	A==not(B),!; % \A > A
	(concept_name_arg(A,NA,TA),concept_name_arg(B,NB,TB),
	    (subsumes(fun(_,TB),TA),!; % A(f(x))>B(x)
		(TA==TB,NA@>NB))).
   	

/******************* Rendezett rezolucios kovetkeztetesi lepes *********************/

% resolve(+C1,+C2,-R): R a C1 ‚és C2 klozok rezolvense
resolve([Res1|Cls1],C2,R):-
	copy_term(C2,R2),R2=[Res2|Cls2],      % valtozok szeparalasa
	(not(Res1)=Res2,!;             	      % nincs szukseg elofordulas-ellenorzesre,
	    Res1=not(Res2)),                  % hiszen szeparaltuk a valtozokat
	append(Cls1,Cls2,L),sort(L,EL),
	cls_to_ocls(EL,R).


/*********************** Redundans klozok elhagyasa *******************************/

% not_true(+Cls): Igaz, ha a Cls kloz nem tartalmazza egyidejuleg 
% valamely literalt es annak negaltjat is
not_true([not(L1)|Ls]):-
	!,\+ (member(L2,Ls),L1==L2),
	not_true(Ls).
not_true([L1|Ls]):-
	\+ (member(not(L2),Ls),L1==L2),
	not_true(Ls).
not_true([]).

% not_redundant(Cls, Clauses): igaz, ha nincs Cls-nel szukebb kloz a Clauses
% klozhalmazban
not_redundant(Cls,Clauses):-
	\+ (member(General,Clauses),includes(Cls,General)).

% elim_reds(Clss,ECs): Clss klozhalmazbol a redundans klozokat elhagyva kapjuk
% az ECs klozhalmazt
elim_reds(Clss,ECs):-
	elim_reds(Clss,[],ECs).

% elim_reds(Clss,Cl2,ECs): Clss klozlistabol elhagyva azokat, amelyeknel szukebb
% kloz talalhato Clss vagy Cl2 klozlistaban, az ECs klozhalmazt kapjuk
elim_reds([C|Cs],Clss,ECs):-
	((not_redundant(C,Cs),not_redundant(C,Clss)) ->
	    ECs=[C|ECs2],Clss2=[C|Clss];
	    ECs=ECs2,Clss2=Clss),
	elim_reds(Cs,Clss2,ECs2).
elim_reds([],_,[]).


% elim_reds2(Clss,Cl2,ECs): Clss klozlistabol elhagyva azokat a klozokat, 
% amelyeknel szukebb kloz talalhato Cl2 klozlistaban, az ECs klozhalmazt kapjuk
elim_reds2([C|Cs],Clss,ECs):-
	(not_redundant(C,Clss) ->
	    ECs=[C|ECs2];
	    ECs=ECs2),
	elim_reds2(Cs,Clss,ECs2).
elim_reds2([],_,[]).

% includes(+C1,+C2): C1 kloz tartalmazza C2-t
% ha C1 legfeljebb 2, C2 csak egy valtozot tartalmaz
includes(C1,C_2):-
	length(C1,L1),length(C_2,L2),L1>=L2,
	copy_term(C_2,C2),
	C1=[H1|_],C2=[H2|_],
	term_variables(H1,[X|Ys]),
	(term_variables(H2,[X]);
	    term_variables(H2,Ys)),
	list_to_ord_set(C1,S1),list_to_ord_set(C2,S2),
	ord_subset(S2,S1),!.
	

/***************************** Klozhalmaz telitese **********************************/

% saturate(+C1,+C2,-S): C1 es C2 "rendezett klozok" listaja, S-t C1 es C2
% telitesevel nyerjuk ugy hogy C1 es C2-beli klozokat, valamint C2-C2-beli
% klozokat rezolvalunk egymassal
saturate(C1,C2,S):-
	append(C1,C2,C3),
	findall(R,(
		   select(R1,C2,M),
		   (member(R2,C1);      % Ujat a regivel
		       member(R2,M)),   % Ujat az ujjal
		   resolve(R1,R2,R),not_redundant(R,C3)
		  ),Rs),
	elim_reds(Rs,ERs),
	(ERs=[]->S=C3;
	    elim_reds2(C3,ERs,EC),saturate(EC,ERs,S)).

/*************** Adott strukturaju klozok elhagyasa a klozhalmazbol *******************/

% contains_struct(X,S): X tartalmaz S alaku strukturat
contains_struct(X,S):-
	copy_term(S,F),subsumes(F,X),!.
contains_struct(X,S):-
	nonvar(X),X=..[_|Args],any_contains_struct(Args,S).

% any_contains_struct(Xs,S): Xs lista valamelyik eleme tartalmaz S alaku sturkturat
any_contains_struct([X|_],S):-
	contains_struct(X,S),!.
any_contains_struct([_|Xs],S):-
	any_contains_struct(Xs,S).

% omit_structs(Cs,S,Ofs): Ofs listat ugy kapjuk, hogy a Cs klozlistabol
% elhagyjuk az S alaku strukturat tartalmazokat
omit_structs([C|Cs],S,Os):-
	contains_struct(C,S),!,omit_structs(Cs,S,Os).
omit_structs([C|Cs],S,[C|Os]):-
	omit_structs(Cs,S,Os).
omit_structs([],_,[]).


/****************************************************************************************/
/*************** Az ujonnan bevezetett fogalmak eliminalasa *****************************/
/****************************************************************************************/

% resolveq(+Clause1,Clause2,Resolvant): Resolvant Clause1 es Clause2 kloz valamelyik uj
% fogalom szerinti rezolvense
resolveq(Res1,C2,R):-
	copy_term(C2,Res2),
	(select(nconcept(Q,X),Res1,M1),select(not(nconcept(Q,X)),Res2,M2);
	    select(not(nconcept(Q,X)),Res1,M1),select(nconcept(Q,X),Res2,M2)),
	append(M1,M2,L),sort(L,R),
	not_true(R).

% saturateq(+C1,+C2,-S): C1 es C2 klozok listaja, S-t C1 es C2
% telitesevel nyerjuk ugy hogy C1 es C2-beli klozokat, valamint C2-C2-beli
% klozokat rezolvalunk egymassal uj fogalom szerint
saturateq(C1,C2,S):-
	append(C1,C2,C3),
	none_recursive(C3),
	findall(R,(
		   (member(R1,C1),member(R2,C2);
		       suffix([R1,M1|Ms],C2),member(R2,[M1|Ms])),
		   resolveq(R1,R2,R),not_redundantq(R,C3)
		  ),Rs),
	elim_redsq(Rs,ERs),
	(ERs=[]->S=C3;
	    elim_reds2q(C3,ERs,EC),saturateq(EC,ERs,S)).


% none_recursive(+Clss): Igaz, ha a Clss klozlista nem tartalmaz rekurziv klozt
none_recursive([Clause|Clauses]):-
	not_recursive(Clause),
	none_recursive(Clauses).
none_recursive([]).
	
% not_recursive(+Cls): Igaz, ha a Cls kloz nem tartalmazza egyidejuleg valamely literalt,
% es annak negaltjat is (termeszetesen mas valtozoval)
not_recursive([not(nconcept(Q,_))|Ls]):-
	!,\+ member(nconcept(Q,_),Ls),
	not_recursive(Ls).
not_recursive([nconcept(Q,_)|Ls]):-
	!,\+ member(not(nconcept(Q,_)),Ls),
	not_recursive(Ls).
not_recursive([_|Ls]):-
	not_recursive(Ls).
not_recursive([]).


/*********************** Redundans klozok elhagyasa *******************************/

% not_redundantq(Cls, Clauses): igaz, ha nincs Cls-nel szukebb kloz a Clauses
% klozhalmazban
not_redundantq(Cls,Clauses):-
	\+ (member(General,Clauses),includesq(Cls,General)).

% elim_redsq(Clss,ECs): Clss klozhalmazbol a redundans klozokat elhagyva kapjuk
% az ECs klozhalmazt
elim_redsq(Clss,ECs):-
	elim_redsq(Clss,[],ECs).

% elim_redsq(Clss,Cl2,ECs): Clss klozlistabol elhagyva azokat, amelyeknel szukebb
% kloz talalhato Clss vagy Cl2 klozlistaban, az ECs klozhalmazt kapjuk
elim_redsq([C|Cs],Clss,ECs):-
	((not_redundantq(C,Cs),not_redundantq(C,Clss)) ->
	    ECs=[C|ECs2],Clss2=[C|Clss];
	    ECs=ECs2,Clss2=Clss),
	elim_redsq(Cs,Clss2,ECs2).
elim_redsq([],_,[]).


% elim_reds2q(Clss,Cl2,ECs): Clss klozlistabol elhagyva azokat a klozokat, amelyeknel szukebb
% kloz talalhato Cl2 klozlistaban, az ECs klozhalmazt kapjuk
elim_reds2q([C|Cs],Clss,ECs):-
	(not_redundantq(C,Clss) ->
	    ECs=[C|ECs2];
	    ECs=ECs2),
	elim_reds2q(Cs,Clss,ECs2).
elim_reds2q([],_,[]).

% includesq(C1,C2): igaz, ha a C1 kloz tartalmazza a C2 klozt
includesq(C1,C_2):-
	copy_term(C_2,C2),
	term_variables(C1,V1),term_variables(C2,V2),
	length(V1,L1),length(V2,L2),L1>=L2,
	assignvars(V1,V2),
	list_to_ord_set(C1,S1),
	list_to_ord_set(C2,S2),
	ord_subset(S2,S1),!.

% assignvars(V1,V2): A V2 listaban levo valtozokhoz V1-beli valtozokat rendel
assignvars(V1,[V|Vs]):-
	select(V,V1,VR),
	assignvars(VR,Vs).
assignvars(_,[]).


/**********************************************************************************/
/**************************** A tenyleges forditas ********************************/
/**********************************************************************************/

% axioms_to_clauses(+Axioms,-RClauses): RClauses az Axioms terminologiai axiomak
% forditasabol kapott klozhalmaz
axioms_to_clauses(Axioms,RClauses):-
	axiomsToNNFConcepts(Axioms,NNF),                % belsosites es negacios normalformara hozas
	defNormForms(NNF,Defs),                         % strukturalis transzformacio
	toFOLList(Defs,_,FOL),                          % lekepezes skolemizalt elsorendu formulakra
	list_cls(FOL,FOLClauses),                       % klozformara hozas
	findall(X,(member(FOLCls,FOLClauses),cls_to_ocls(FOLCls,X)),OrderedClss), % literalok kijelolese
	saturate([],OrderedClss,Saturated),             % rendezett rezolucio
	omit_structs(Saturated,fun(_,_),FunFree),       % fuggvenyjelet tartalmazo klozok elhagyasa
	(saturateq([],FunFree,SatQ)->                   % uj fogalmak kikuszobolese
	    omit_structs(SatQ,nconcept(_,_),RClauses);  %   ha sikerult, elhagyhatjuk azokat a klozokat
	    RClauses = FunFree).                        %   kulonben az eredeti klozokat adjuk vissza

