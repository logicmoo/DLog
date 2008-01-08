:- module(translator,[axioms_to_clauses/2,axioms_to_clauses/4]).

:- use_module(show).
:- use_module(struct).
:- use_module(dl_to_fol).
:- use_module(transitive).
:- use_module(saturate, [saturate/2, remove_redundant/2, remove_temp/2]).
:- use_module(saturate_without_binary).
:- use_module(library(lists), [append/3,select/3, member/2]).

% axioms_to_clauses(+Axioms,-Clauses): Clauses az Axioms SHIQ terminologiai axiomak
% forditasabol kapott klozhalmaz
axioms_to_clauses(SHIQAxioms,Clauses):-
	collectInverses(SHIQAxioms,Ibox),
	
	separateBoxes(SHIQAxioms,Tbox,Hbox,Trbox),
	addTransitiveInverses(Trbox,Ibox,Trbox2),
	
	% nl,print('Eredeti Tbox'),nl,
	% nl,nl, show(Tbox),nl,nl,

	
	removeTransitive(Tbox,Hbox,Trbox2,ALCHIQTbox),
	
	% nl,print('ALCHIQ Tbox'),nl,
	% nl,nl, show(ALCHIQTbox),nl,nl,
	
	% nl,print('Hbox'),nl,
	% nl,nl, show(Hbox),nl,nl,
	
	% nl,print('Ibox'),nl,
	% nl,nl, show(Ibox),nl,nl,
	
	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(ALCHIQTbox,NNF),				
	
	% nl,print('NNF'),nl,
	% nl,nl, show(NNF),nl,nl,
	
	% strukturalis transzformacio
	defNormForms(NNF,Defs),
	
	% nl,print('Strukturalis transzformacio'),nl,
	% nl,nl, show(Defs),nl,nl,		
	
	% elsorendu, skolemizalt formulak kepzese
	toFOLList(Defs,_,FOLT),
	toFOLList(Hbox,_,FOLH),
	toFOLList(Ibox,_,FOLI),
		
	append(FOLT,FOLH,FOL2),
	append(FOL2,FOLI,FOL),			
	
	% clozformara hozas
	list_cls(FOL,FOLClauses),
	
	% nl,print('FOL forma'),nl,
	% nl,nl, show(FOLClauses),nl,nl,

	
	% nl,print('Klozok telitese'),nl,

	
	% klozhalmaz telitese alap-szuperpozicioval
	saturate(FOLClauses,Saturated),	
	
	% nl,print('Telites utan'),nl,
	% nl,nl, show(Saturated),nl,nl,
	
	% kettos fuggvenyjelet tartalmazo klozok elhagyasa
	omit_structs(Saturated,fun(_,marked(fun(_,_))),Reduced1),

	% fuggveny egyenloseg/egyenlotlensegek elhagyasa
	omit_structs(Reduced1,eq(marked(fun(_,_)),_),Reduced2),
	omit_structs(Reduced2,not(eq(_,_)),Reduced3),
	
	% fuggvenyjeles szerepek elhagyasa
	omit_structs(Reduced3,arole(_,marked(fun(_,_)),_),Reduced4),
	omit_structs(Reduced4,arole(_,X,fun(_,X)),Reduced5),
	omit_structs(Reduced5,arole(_,X,marked(fun(_,X))),Reduced),
	
	% nl,print('Mindenfele elhagyas utan'),nl,
	% nl,nl, show(Reduced),nl,nl,
	
	% fuggvenyek kikuszobolese
	saturate_without_binary(Reduced,FunFree1),
	omit_structs(FunFree1, fun(_,_),FunFree),
	
	% nl,print('Fuggvenyjelek kikuszobolese utan'),nl,
	% nl,nl, show(FunFree),nl,nl,
	
	remove_temp(FunFree,Removed1),
	
	% nl,print('Bevezetett fogalmak kikuszobolese utan'),nl,
	% nl,nl, show(Removed1),nl,nl,

	remove_double_proof_list(Removed1,Removed),
	% remove_redundant(Removed2,Removed),

	% nl,print('Kettos bizonyitasok kikuszobolese utan'),nl,
	% nl,nl, show(Removed),nl,nl,	
	Clauses = Removed.
	

axioms_to_clauses(SHIQAxioms,Clauses,Ibox,Hbox):-
	collectInverses(SHIQAxioms,Ibox),
	
	separateBoxes(SHIQAxioms,Tbox,Hbox,Trbox),
	addTransitiveInverses(Trbox,Ibox,Trbox2),
	
	% nl,print('Eredeti Tbox'),nl,
	% nl,nl, show(Tbox),nl,nl,
	
	removeTransitive(Tbox,Hbox,Trbox2,ALCHIQTbox),
	
	% nl,print('ALCHIQ Tbox'),nl,
	% nl,nl, show(ALCHIQTbox),nl,nl,
	
	% nl,print('Hbox'),nl,
	% nl,nl, show(Hbox),nl,nl,
	
	% nl,print('Ibox'),nl,
	% nl,nl, show(Ibox),nl,nl,
	
	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(ALCHIQTbox,NNF),				
	
	% nl,print('NNF'),nl,
	% nl,nl, show(NNF),nl,nl,
	
	% strukturalis transzformacio
	defNormForms(NNF,Defs),
	
	% nl,print('Strukturalis transzformacio'),nl,
	% nl,nl, show(Defs),nl,nl,		
	
	% elsorendu, skolemizalt formulak kepzese
	toFOLList(Defs,_,FOL),
		
	% clozformara hozas
	list_cls(FOL,FOLClauses),
	
	% nl,print('FOL forma'),nl,
	% nl,nl, show(FOLClauses),nl,nl,

	
	% nl,print('Klozok telitese'),nl,

	
	% klozhalmaz telitese alap-szuperpozicioval
	saturate(FOLClauses,Saturated),	
	
	% nl,print('Telites utan'),nl,
	% nl,nl, show(Saturated),nl,nl,
	
	% kettos fuggvenyjelet tartalmazo klozok elhagyasa
	omit_structs(Saturated,fun(_,marked(fun(_,_))),Reduced1),

	% fuggveny egyenloseg/egyenlotlensegek elhagyasa
	omit_structs(Reduced1,eq(marked(fun(_,_)),_),Reduced2),
	omit_structs(Reduced2,not(eq(_,_)),Reduced3),
	
	% fuggvenyjeles szerepek elhagyasa
	omit_structs(Reduced3,arole(_,marked(fun(_,_)),_),Reduced4),
	omit_structs(Reduced4,arole(_,X,fun(_,X)),Reduced5),
	omit_structs(Reduced5,arole(_,X,marked(fun(_,X))),Reduced),
	
	% nl,print('Mindenfele elhagyas utan'),nl,
	% nl,nl, show(Reduced),nl,nl,
	
	% fuggvenyek kikuszobolese
	saturate_without_binary(Reduced,FunFree1),
	omit_structs(FunFree1, fun(_,_),FunFree),
	
	% nl,print('Fuggvenyjelek kikuszobolese utan'),nl,
	% nl,nl, show(FunFree),nl,nl,
	
	remove_temp(FunFree,Removed1),
	
	% nl,print('Bevezetett fogalmak kikuszobolese utan'),nl,
	% nl,nl, show(Removed1),nl,nl,

	remove_double_proof_list(Removed1,Removed),
	% remove_redundant(Removed2,Removed),

	% nl,print('Kettos bizonyitasok kikuszobolese utan'),nl,
	% nl,nl, show(Removed),nl,nl,	
	Clauses = Removed.
	


	





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

	
	
/**************************************************************************/
/************************* Teszteles **************************************/
/**************************************************************************/

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