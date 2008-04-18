:- module(translator_fol,[translate_axioms_fol/4]).

:- use_module('../show').
:- use_module('../struct').
:- use_module('dl_to_fol_fol').
:- use_module('transitive_fol').
:- use_module('saturate_fol', [saturate/2, saturate_partially/3, remove_redundant/2, remove_temp/2]).
:- use_module('saturate_without_binary_fol').
:- use_module(library(lists), [append/3,select/3, member/2]).

translate_axioms_fol(SHIQAxioms,Clauses,Hbox,Transitive):-
	translate_axioms2(SHIQAxioms,Clauses1,_Ibox,Hbox,Transitive),
	% tipusmegjelolesek elhagyasa
	% inverzek es szerephierarchiak elhagyasa	
	findall(C,(
		   member([Type,C],Clauses1);
		   \+ Type = 1
		  ), Clauses
	       ).


translate_axioms2([Tbox,Hbox,Trbox],Clauses,Ibox,Hbox,Trbox2):-
	% nl,print('Eredeti KB'),nl,nl, show(Tbox),nl,nl,

	replace_inv_list(Tbox,Tbox2),

	collectInverses(Tbox2,Ibox),

	addTransitiveInverses(Trbox,Ibox,Trbox2),

	% nl,print('Eredeti Tbox'),nl,
	% nl,nl, show(Tbox2),nl,nl,

	removeTransitive(Tbox2,Hbox,Trbox2,TransTbox),
	
	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(Tbox2,NNF),
	axiomsToNNFConcepts(TransTbox,TransNNF),	
	
	% nl,print('Negacios normalformara hozas utan'),nl,nl, show(NNF),nl, show(TransNNF), nl,nl,

	% strukturalis transzformacio
	def_list(NNF,'n_',Defs),
	def_list(TransNNF,'trans_',TransDefs),

	append(Defs,TransDefs,AllDefs),
	
	% nl,print('Strukturalis transzformacio utan'),nl,nl, show(AllDefs),nl,nl,
	
	% elsorendu, skolemizalt formulak kepzese
	toClauseList(AllDefs,FOLT),
	toClauseList(Hbox,FOLH),
	toClauseList(Ibox,FOLI),
		
	append(FOLT,FOLH,FOL2),
	append(FOL2,FOLI,FOL),

	% nl,print('FOL klozok kepzese'),nl,nl, show(FOL),nl,nl,

	saturate(FOL,Saturated),
	
	% nl,print('Telites utan'),nl,nl, show(Saturated),nl,nl,	

	omit_structs(Saturated,fun(_,_),FunFree),

        % nl,print('Fuggvenyjelek kikuszobolesevel'),nl,nl, show(FunFree),nl,nl,

	remove_temp(FunFree,Removed),
	
	% nl,print('Bevezetett fogalmak kikuszobolese utan'),nl,nl, show(Removed),nl,nl,

	remove_double_proof_list(Removed,Removed2),

	% nl,print('Kettos bizonyitasok kikuszobolese utan'),nl,nl, show(Removed2),nl,nl,

	remove_redundant(Removed2,Clauses),

	% nl,print('Vegso redundanciavizsgalat utan'),nl,nl, show(Clauses),nl,nl,
	true.
	

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