:- module(translator,[axioms_to_clauses/5, axioms_to_clauses/2]).

:- use_module(library(lists), [append/3,select/3, member/2]).
:- use_module(transitive).
:- use_module(dl_to_fol).
:- use_module(saturate, [saturate/3]).
:- use_module(toFOL, [toClause_list/2]).
:- use_module(show).
:- use_module(struct).

% :- use_module(saturate_without_binary).

% axioms_to_clauses(+Axioms,-Clauses,-Ibox,-Hbox,-Trbox): Clauses az Axioms SHIQ terminologiai axiomak
% forditasabol kapott klozhalmaz
% Ibox az inverzeket tartalmazza
% Hbox a szerephierarchiat
% Trbox a tranzitiv szerepeket
axioms_to_clauses(SHIQAxioms,Clauses,_Ibox,Hbox,Transitive):-
	axioms_to_clauses2(SHIQAxioms,Clauses1,Hbox,Transitive),
	% elhagyjuk a tranzitivitashoz tartozo klozokat
	findall(C,(
		   member(C,Clauses1),
%		   \+ (
%			contains_struct2(C,nconcept(Pred,_)),
%			atom_concat('trans',_,Pred)
%		      ),
		   true
		  ), Clauses	       
	       ).

axioms_to_clauses(SHIQAxioms,Clauses):-
	axioms_to_clauses2(SHIQAxioms,Clauses,_,_).


% elso argumentum egy harmas lista: [CInclusion, RInclusion, Transitive]
axioms_to_clauses2([CInclusion, RInclusion, Transitive],Clauses,RInclusion,Transitive):-

	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(CInclusion,NNF),
	
	removeTransitive(NNF,RInclusion,Transitive,TransNNF),
	
	% nl,print('NNF'),nl, show(NNF),nl, show(TransNNF), nl,

	% strukturalis transzformacio
	def_list(NNF,'n_',Defs),		
	def_list(TransNNF,'trans_',TransDefs),

	append(Defs,TransDefs,AllDefs),
	
	% nl,print('Strukturalis transzformacio utan'),nl, show(AllDefs),nl,

	% nl,print('Klozok telitese'),nl,	
	% klozhalmaz telitese alap-szuperpozicioval
	saturate(AllDefs,RInclusion,Saturated),
	
	% nl,print('Telites utan'),nl, show(Saturated),nl,

	omit_structs(Saturated,atleast(_,_,_,_),FunFree),

        % nl,print('Fuggvenyjelek kikuszobolesevel'),nl, show(FunFree),nl,

	toClause_list(FunFree,FOL),

	% remove_temp(FunFree,Removed),
	
	% nl,print('Elsorendu klozok kepzese'),nl,nl, show(FOL),nl,

	% remove_double_proof_list(Removed,Removed2),

	% nl,print('Kettos bizonyitasok kikuszobolese utan'),nl,
	% nl,nl, show(Removed2),nl,nl,

	% remove_redundant(Removed2,Clauses),

	% nl,print('Vegso redundanciavizsgalat utan'),nl,
	% nl,nl, show(Clauses),nl,nl,
	Clauses = FOL.
	

/**************************************************************************/
/******************* Ketszeres bizonyitas elkerulese **********************/
/**************************************************************************/
					      
% remove_double_proof_list(+L,-S)
% Ha az L beli klozban vannak olyan literalparok, melyek eliminalasa ugyanannak
% a bizonyitasnak az ismetleset jelentene, akkor kiszurjuk az egyiket
% a keletkezo klozlista S
remove_double_proof_list([],[]).
remove_double_proof_list([[T,L]|Ls],[[T,R]|Rs]):-
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


filter(L,R):-
	append(F1,F2,L),
	length(F1,108),
	append(F21,F22,F2),
	length(F22,11),


	findall(C, (
		     member(C,F21),
		     (
		       contains_struct(C,arole('IsLoanOf',_,_))
		     ; contains_struct(C,arole('inv_IsLoanOf',_,_))		     
		     ; contains_struct(C,fun(k,_))
		     ; contains_struct(C,fun(j,_))
		     ; contains_struct(C,fun(i,_))
		     ; \+ contains_struct(C,fun(_,_))
		     ),
		     \+ contains_struct(C,aconcept('Region',_)),
		     \+ contains_struct(C,arole('livesIn',_,_))
		   ), R
	       ).
	
	
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