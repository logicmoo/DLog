:- module(translator,[axioms_to_clauses/5]).

:- use_module(show).
:- use_module(struct).
:- use_module(dl_to_fol).
:- use_module(transitive).
:- use_module(saturate, [saturate/2, saturate_partially/3, remove_redundant/2, remove_temp/2]).
:- use_module(saturate_without_binary).
:- use_module(library(lists), [append/3,select/3, member/2]).

% axioms_to_clauses(+Axioms,-Clauses,-Ibox,-Hbox,-Trbox): Clauses az Axioms SHIQ terminologiai axiomak
% forditasabol kapott klozhalmaz
% Ibox az inverzeket tartalmazza
% Hbox a szerephierarchiat
% Trbox a tranzitiv szerepeket
axioms_to_clauses(SHIQAxioms,Clauses,Ibox,Hbox,Trbox2):-
	% nl,print('Eredeti KB'),nl,
	% nl,nl, show(SHIQAxioms),nl,nl,
	
	collectInverses(SHIQAxioms,Ibox),

	separateBoxes(SHIQAxioms,Tbox,Hbox,Trbox),
	addTransitiveInverses(Trbox,Ibox,Trbox2),

	% nl,print('Eredeti Tbox'),nl,
	% nl,nl, show(Tbox),nl,nl,

	removeTransitive(Tbox,Hbox,Trbox2,TransTbox),
	
	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(Tbox,NNF),
	axiomsToNNFConcepts(TransTbox,TransNNF),	
	
	% nl,print('Negacios normalformara hozas utan'),nl,	
	% nl,nl, show(NNF),nl, show(TransNNF), nl,nl,

	% strukturalis transzformacio
	def_list(NNF,'n_',Defs),
	def_list(TransNNF,'trans_',TransDefs),

	append(Defs,TransDefs,AllDefs),
	
	% nl,print('Strukturalis transzformacio utan'),nl,
	% nl,nl, show(AllDefs),nl,nl,
	
	% elsorendu, skolemizalt formulak kepzese
	toClauseList(AllDefs,FOLT),
	toClauseList(Hbox,FOLH),
	toClauseList(Ibox,FOLI),
		
	append(FOLT,FOLH,FOL2),
	append(FOL2,FOLI,FOL),			

	% nl,print('FOL klozok kepzese'),nl,
	% nl,nl, show(FOL),nl,nl,

	% nl,print('Klozok telitese'),nl,	
	% klozhalmaz telitese alap-szuperpozicioval
	saturate(FOL,Saturated),
	
	% nl,print('Telites utan'),nl,
	% nl,nl, show(Saturated),nl,nl,	

	omit_structs(Saturated,fun(_,_),FunFree1),
	omit_structs(FunFree1,[1,_],FunFree),

        % nl,print('Fuggvenyjelek kikuszobolesevel'),nl,
	% nl,nl, show(FunFree),nl,nl,

	remove_temp(FunFree,Removed),
	
	% nl,print('Bevezetett fogalmak kikuszobolese utan'),nl,
	% nl,nl, show(Removed),nl,nl,

	remove_double_proof_list(Removed,Removed2),
	remove_redundant(Removed2,Removed3),

	% nl,print('Kettos bizonyitasok kikuszobolese utan'),nl,
	% nl,nl, show(Removed3),nl,nl,

	% tranzitivitashoz kotodo axiomak es a tipusmegjelolesek elhagyasa
	findall(C,(
		   member([_,C],Removed3),		   
%		   \+ (
%			contains_struct2(C,nconcept(Pred,_)),
%			atom_concat('trans',_,Pred)
%		      )
		   true
		  ), Clauses
	       ),

	
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

% separate5(+L,-Fiveless,-Five): L klozokbol az 5 tipusuak Five-ban
% vannak, a tobbi pedig Fiveless-ben
separate5([],[],[]).
separate5([[5,C]|L],Fiveless,[[5,C]|Five]):-
	!, separate5(L,Fiveless,Five).
separate5([C|L],[C|Fiveless],Five):-
	separate5(L,Fiveless,Five).

	
	
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