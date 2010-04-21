:- module(translator,[translate_axioms/2]).

:- use_module('../show').
:- use_module('../struct').
:- use_module('transitive').
:- use_module('nnf',[axiomsToNNFConcepts/2]).
:- use_module('structural',[def_list/2]).
:- use_module('dl_to_fol').
:- use_module('saturate',[saturate/2,remove_temp/2,remove_redundant/2]).
:- use_module('remove_double_proof', [remove_double_proof_list/2]).

% :- use_module('saturate_without_binary_shoiq').

translate_axioms([Tbox,Hbox,Trbox],Clauses):-
	nl,print('Eredeti KB'),nl, show(Tbox),nl,nl,

	collectInverses(Tbox,Ibox),
	addTransitiveInverses(Trbox,Ibox,Trbox2),

	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(Tbox,NNF),
	nl,print('Negacios normalformara hozas utan'),nl, show(NNF),nl,nl,
	
	removeTransitive(NNF,Hbox,Trbox2,TransTbox),
	nl,print('Tranzitivitasbol szarmazo bennsositett fogalmak'),nl, show(TransTbox),nl,nl,	

	% tranzitivitasbol szarmazo axiomakat hozzaadjuk az eredeti axiomakhoz
	append(NNF,TransTbox,NNF2),
		
	% strukturalis transzformacio
	def_list(NNF2,Defs),
	
	nl,print('Strukturalis transzformacio utan'),nl,nl, show(Defs),nl,nl,

	% elsorendu, skolemizalt formulak kepzese
	iboxToClauseList(Ibox,FOLI),
	hboxToClauseList(Hbox,FOLH),
	tboxToClauseList(Defs,FOLT),
		
	append(FOLT,FOLH,FOL2),
	append(FOL2,FOLI,FOL),

	nl,print('FOL klozok kepzese'),nl,nl, show(FOL),nl,nl,

	length(FOL1,6),
	append(FOL1,_,FOL),
	saturate(FOL,Saturated),
	
	nl,print('Telites utan'),nl,nl, show(Saturated),nl,nl,

	omit_structs(Saturated,fun(_,_,_),FunFree),

        nl,print('Fuggvenyjelek kikuszobolesevel'),nl, show(FunFree),nl,nl,

	remove_temp(FunFree,Removed),
	
	nl,print('Bevezetett fogalmak kikuszobolese utan'),nl, show(Removed),nl,nl,

	remove_double_proof_list(Removed,Removed2),

	nl,print('Kettos bizonyitasok kikuszobolese utan'), nl, show(Removed2),nl,nl,

	remove_redundant(Removed2,Clauses),

	nl,print('Vegso redundanciavizsgalat utan'),nl, show(Clauses),nl,nl,
	true.