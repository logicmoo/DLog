:- module(translator_old,[translate_axioms_old/4]).

:- use_module('../show').
:- use_module('../struct').
:- use_module(dl_to_fol_old).
:- use_module(transitive_old).
:- use_module(saturate_old, [saturate/2, remove_redundant/2, remove_temp/2]).
:- use_module(saturate_without_binary_old).
:- use_module(library(lists), [append/3,select/3, member/2]).

% axioms_to_clauses(+Axioms,-Clauses,-Ibox,-Hbox,-Trbox): Clauses az Axioms SHIQ terminologiai axiomak
% forditasabol kapott klozhalmaz
% Ibox az inverzeket tartalmazza
% Hbox a szerephierarchiat
% Trbox a tranzitiv szerepeket
translate_axioms_old([Tbox,Hbox,Trbox],Clauses,Hbox2,Trbox3):-

	replace_inv_list(Tbox,Tbox2),
	replace_inv_list(Hbox,Hbox2),
	replace_inv_list(Trbox,Trbox2),

	collectInverses(Tbox2,Ibox),
	addTransitiveInverses(Trbox2,Ibox,Trbox3),

	% nl,print('Eredeti Tbox'),nl,nl, show(Tbox),nl,nl,

	removeTransitive(Tbox2,Hbox2,Trbox3,TransTbox),
	
	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(Tbox2,NNF),
	axiomsToNNFConcepts(TransTbox,TransNNF),	
	
	% nl,print('Negacios normalformara hozas utan'),nl,nl, show(NNF),nl, show(TransNNF), nl,nl,
		
	% strukturalis transzformacio
	def_list(NNF,'n_',Defs),
	def_list(TransNNF,trans_,TransDefs),
	% defNormForms(NNF,newaaa,Defs),
	% defNormForms(TransNNF,transaaa,TransDefs),

	append(Defs,TransDefs,AllDefs),
	
	% nl,print('Strukturalis transzformacio utan'),nl,nl, show(AllDefs),nl,nl,
	
	% elsorendu, skolemizalt formulak kepzese
	toFOLList(AllDefs,_,FOLT),
	toFOLList(Hbox2,_,FOLH),
	toFOLList(Ibox,_,FOLI),
		
	append(FOLT,FOLH,FOL2),
	append(FOL2,FOLI,FOL),			

	% nl,print('Elsorendu logikaban'),nl,nl, show(FOL),nl,nl,
	
	% clozformara hozas
	list_cls(FOL,FOLClauses),

	% clozok tipusokba sorolasa
	clause_types(FOLClauses,TypedClauses),
	
	% nl,print('FOL klozokra alakitas utan'),nl,nl, show(TypedClauses),nl,nl,

	% nl,print('Klozok telitese'),nl,
	
	% klozhalmaz telitese alap-szuperpozicioval
	saturate(TypedClauses,Saturated),
	
	% nl,print('Telites utan'),nl,nl, show(Saturated),nl,nl,
	
	omit_structs(Saturated,fun(_,_),FunFree),

	% nl,print('Fuggvenyjelek kikuszobolesevel'),nl,nl, show(FunFree),nl,nl,

	findall(C,(
		   member([T,C],FunFree),
		   T \== 1
		  ), Typeless
	       ),	   

	% nl,print('Szerephierarchia es inverzek elhagyasaval'),nl,nl, show(Typeless),nl,nl,
	
	remove_temp(Typeless,Removed),	
	
	% nl,print('Bevezetett fogalmak kikuszobolese utan'),nl,
	% nl,nl, show(Removed),nl,nl,

	remove_double_proof_list(Removed,NoDouble),
	remove_redundant(NoDouble,Clauses),

	% nl,print('Kettos bizonyitasok kikuszobolese utan'),nl,
	% nl,nl, show(Clauses),nl,nl,
	true.
	

/**************************************************************************/
/******************* Ketszeres bizonyitas elkerulese **********************/
/**************************************************************************/
					      
% remove_double_proof_list(+L,-S)
% Ha az L beli klozban vannak olyan literalparok, melyek eliminalasa ugyanannak
% a bizonyitasnak az ismetleset jelentene, akkor kiszurjuk az egyiket
% a keletkezo klozlista S
remove_double_proof_list([],[]).
remove_double_proof_list([L|Ls],[R|Rs]):-
	remove_double_proof(L,R),
	remove_double_proof_list(Ls,Rs).

% remove_double_proof(+L,-S)
% Ha az L klozban vannak olyan literalparok, melyek eliminalasa ugyanannak
% a bizonyitasnak az ismetleset jelentene, akkor kiszurjuk az egyiket
% a keletkezo kloz S
remove_double_proof(L,S):-
	select(not(arole(R,X,Y1)),L,L1),
	select(not(arole(R,X,Y2)),L1,L2),
	
	findall(C, (
		     ( contains_struct2(L2,aconcept(C,_))
		     ; contains_struct2(L2,nconcept(C,_))
		     ),
		     ( member(M1,L2), M1 == aconcept(C,Y1), \+ (member(M2,L2), M2 == aconcept(C,Y2))
		     ; member(M1,L2), M1 == not(aconcept(C,Y1)), \+ (member(M2,L2), M2 == not(aconcept(C,Y2)))
		     ; member(M1,L2), M1 == nconcept(C,Y1), \+ (member(M2,L2), M2 == nconcept(C,Y2))
		     ; member(M1,L2), M1 == not(nconcept(C,Y1)), \+ (member(M2,L2), M2 == not(nconcept(C,Y2)))
		     ; member(M1,L2), M1 == aconcept(C,Y2), \+ (member(M2,L2), M2 == aconcept(C,Y1))
		     ; member(M1,L2), M1 == not(aconcept(C,Y2)), \+ (member(M2,L2), M2 == not(aconcept(C,Y1)))
		     ; member(M1,L2), M1 == nconcept(C,Y2), \+ (member(M2,L2), M2 == nconcept(C,Y1))
		     ; member(M1,L2), M1 == not(nconcept(C,Y2)), \+ (member(M2,L2), M2 == not(nconcept(C,Y1)))		   
		     )
		     ), Cs
	       ),	
	Cs = [],
	\+ (member(M,L2), (M == eq(Y1,Y2); M == eq(Y2,Y1))), !,
	Y1 = Y2,
	sort(L,LReduced),
	remove_double_proof(LReduced,S).
remove_double_proof(L,L).

% separate50(+L,-Fiftyless,-Fifty): L klozokbol az 50 tipusuak Fifty-ben
% vannak, a tobbi pedig Fiftyless-ben
separate50([],[],[]).
separate50([[50,C]|L],Fiftyless,[[50,C]|Fifty]):-
	!, separate50(L,Fiftyless,Fifty).
separate50([C|L],[C|Fiftyless],Fifty):-
	separate50(L,Fiftyless,Fifty).

	
	
/**************************************************************************/
/************************* Teszteles **************************************/
/**************************************************************************/


replace_inv(X,X):-
	atom(X), !.
replace_inv(inv(arole(R)),arole(R2)):- !,
	atom_concat('inv_',R,R2).
replace_inv(X,Y):-
	X =.. [Head|Tail],
	replace_inv_list(Tail,Tail2),
	Y =.. [Head|Tail2].

replace_inv_list([],[]).
replace_inv_list([L|Ls],[R|Rs]):-
	replace_inv(L,R),
	replace_inv_list(Ls,Rs).

/*
test_redundant1:-		% redundant
	redundant([f(_X)],[[f(_Y)]]).
test_redundant2:-		% redundant
	redundant([f(_X),g(_X)],[[f(_Y)]]).
test_redundant3:-		% redundant
	redundant([f(_X),g(_Y)],[[f(_Y)]]).
test_redundant4:-		% redundant
	redundant([f(_X),g(_X)],[[f(_Y),g(_Y)]]).
test_redundant5:-		% not redundant
	redundant([f(_X),g(_Y)],[[f(_Y),g(_Y)]]).
test_redundant6:-		% redundant
	redundant([f(a(_X)),g(a(_X))],[[f(_Y),g(_Y)]]).
test_redundant7:-		% not redundant
	redundant([f(_X),g(_Y)],[[f(_Y),g(_Y)]]).
test_redundant8:-		% not redundant
	redundant([f(a(_X)),g(_Y)],[[f(_Y),g(_Y)]]).
*/