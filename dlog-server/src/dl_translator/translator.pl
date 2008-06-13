:- module(translator,[translate_axioms/4]).

:- use_module(library(lists), [append/3,select/3, member/2]).
:- use_module(dl_to_fol, [axiomsToNNFConcepts/2, def_list/3]).
:- use_module(transitive, [removeTransitive/4]).
:- use_module(saturate, [saturate/3, saturate_partially/4, saturate_cross/4]).
:- use_module(toFOL, [toClause_list/2]).
:- use_module(show).
:- use_module(struct,[omit_structs/3, contains_struct/2]).

:- use_module('../prolog_translator/prolog_translator_swi_tools', [bb_put/2, bb_get/2]).

% translate_axioms([+CInclusion, +RInclusion, +Transitive],-Clauses,-RInclusion,-Transitive):-
% elso argumentum egy harmas lista: [CInclusion, RInclusion, Transitive], mely egy SHIQ KB-t ir le
translate_axioms([CInclusion, RInclusion, Transitive],Clauses,RInclusion,Transitive):-

	% belsosites es negacios normalformara hozas
	axiomsToNNFConcepts(CInclusion,NNF),
	
	removeTransitive(NNF,RInclusion,Transitive,TransNNF),
	
	% nl,print('NNF'),nl, show(NNF),nl, show(TransNNF), nl,
	
	% strukturalis transzformacio
	def_list(NNF,'n_',Defs),		
	def_list(TransNNF,'trans_',TransDefs),

	append(Defs,TransDefs,AllDefs),
	
	% nl,print('Strukturalis transzformacio utan'),nl, show(AllDefs),nl,

	% szerepeket tartalmazo klozok levalasztasa
	filter(AllDefs,Type1,Rest),

	% bb_put(time,0),	

	
	% klozhalmaz telitese alap-szuperpozicioval
	% statistics(runtime, [T0,_]),
	saturate(Type1,RInclusion,Saturated1),
	% statistics(runtime, [T1,_]),
	% TA is T1-T0,
	
	% nl,print('Elso telites utan'),nl, % show(Saturated1),nl,
	% format('Saturation in ~t~20|~t~3d sec ~n', [TA]),
	% nl, bb_get(time,Time1), print('Red time: '), write(Time1), nl,	
	
	% show(Rest),
	
	% statistics(runtime, [TT0,_]),
	saturate(Rest,RInclusion,Saturated2),
	% statistics(runtime, [TT1,_]),
	% TTA is TT1-TT0,
	
	% nl,print('Masodik telites utan'),nl, % show(Saturated2),nl,
	% format('Saturation in ~t~20|~t~3d sec ~n', [TTA]),
	% nl, bb_get(time,Time2), print('Red time: '), write(Time2), nl,

	% filter2(Saturated2,Filtered2),

	% nl, print('Fogalmas: '), nl, show(Filtered2), nl,

	% statistics(runtime, [TTT0,_]),
	saturate_cross(Saturated1,Saturated2,RInclusion,Saturated),
	% statistics(runtime, [TTT1,_]),
	% TTTA is TTT1-TTT0,
	
	% nl,print('Harmadik telites utan'),nl, show(Saturated),nl,

	% format('Saturation in ~t~20|~t~3d sec ~n', [TTTA]),
	% nl, bb_get(time,Time3), print('Red time: '), write(Time3), nl,

	omit_structs(Saturated,atleast(_,_,_,_),FunFree),

        % nl,print('Fuggvenyjelek kikuszobolesevel'),nl, show(FunFree),nl,

	toClause_list(FunFree,FOL),

	% nl,print('Elsorendu klozok kepzese'),nl,nl, show(FOL),nl,
	Clauses = FOL.


% szerepeket tartalmazo klozok levalasztasa
filter([],[],[]).
filter([L|Ls],Type1,[L|Rest]):-
	contains_struct(L,arole(_)), !,
	filter(Ls,Type1,Rest).
filter([L|Ls],[L|Type1],Rest):-
	filter(Ls,Type1,Rest).



filter2(L,R):-
	findall(C, (
		     member(C,L),
		     (
		       contains_struct(C,atleast(_,arole('food:hasFood'),aconcept(_),[top]))
%		     ; contains_struct(C,atleast(_,_,_,[aconcept(b)|_]))
		     )
		   ), R
	       ).

filter3(L,R):-
	findall(C, (
		     member(C,L),
		     (
		       contains_struct(C,aconcept('food:EdibleThing'))
		     )
		   ), R
	       ).
