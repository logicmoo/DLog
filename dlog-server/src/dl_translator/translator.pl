:- module(translator,[axioms_to_clauses/5, axioms_to_clauses/2]).

:- use_module(library(lists), [append/3,select/3, member/2]).
:- use_module(transitive, [removeTransitive/4]).
:- use_module(dl_to_fol, [axiomsToNNFConcepts/2, def_list/3]).
:- use_module(saturate, [saturate/3]).
:- use_module(toFOL, [toClause_list/2]).
:- use_module(show).
:- use_module(struct,[omit_structs/3]).

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

	% klozhalmaz telitese alap-szuperpozicioval
	saturate(AllDefs,RInclusion,Saturated),
	
	% nl,print('Telites utan'),nl, show(Saturated),nl,

	omit_structs(Saturated,atleast(_,_,_,_),FunFree),

        % nl,print('Fuggvenyjelek kikuszobolesevel'),nl, show(FunFree),nl,

	toClause_list(FunFree,FOL),

	% nl,print('Elsorendu klozok kepzese'),nl,nl, show(FOL),nl,
	Clauses = FOL.
	
