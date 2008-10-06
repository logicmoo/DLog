:- module(translator_pure,[translate_axioms_pure/2]).

:- use_module(library(lists), [append/3,select/3, member/2]).
:- use_module(dl_to_fol_pure, [axiomsToNNFConcepts/2, def_list/3]).
:- use_module(transitive_pure, [removeTransitive/4]).
:- use_module(saturate_pure, [saturate/3, saturate_partially/4]).
:- use_module(toFOL_pure, [toClause_list/2]).
:- use_module('../show').
:- use_module('../struct',[omit_structs/3, contains_struct/2]).

% translate_axioms_pure([+CInclusion, +RInclusion, +Transitive],-Clauses,-RInclusion,-Transitive):-
% elso argumentum egy harmas lista: [CInclusion, RInclusion, Transitive], mely egy SHIQ KB-t ir le
translate_axioms_pure([CInclusion, RInclusion, Transitive],Clauses):-

	nl, show(CInclusion),nl, show(RInclusion), nl, show(Transitive), nl,
	print('vege'), nl,
	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(CInclusion,NNF),	
	
	removeTransitive(NNF,RInclusion,Transitive,TransNNF),
	
	nl,print('NNF'),nl, show(NNF),nl, show(TransNNF), nl,
	
	% strukturalis transzformacio
	def_list(NNF,'n_',Defs),		
	def_list(TransNNF,'trans_',TransDefs),

	append(Defs,TransDefs,AllDefs),
	
	% nl,print('Strukturalis transzformacio utan'),nl, show(AllDefs),nl,

	% szerepeket tartalmazo klozok levalasztasa
%	separate(AllDefs,Type1,Rest),

	% klozhalmaz telitese alap-szuperpozicioval
	saturate(AllDefs,RInclusion,Saturated),
	
	% nl,print('Telites utan'),nl, show(Saturated),nl,	

	omit_structs(Saturated,atleast(_,_,_),FunFree),

        % nl,print('Fuggvenyjelek kikuszobolesevel'),nl, show(FunFree),nl,

	toClause_list(FunFree,FOL),

	nl,print('Elsorendu klozok kepzese'),nl,nl, show(FOL),nl,
	Clauses = FOL.


% szerepeket tartalmazo klozok levalasztasa
separate([],[],[]).
separate([L|Ls],Type1,[L|Rest]):-
	contains_struct(L,arole(_)), !,
	separate(Ls,Type1,Rest).
separate([L|Ls],[L|Type1],Rest):-
	separate(Ls,Type1,Rest).