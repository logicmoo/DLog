:- module(axioms_to_clauses, [axioms_to_clauses/6, axioms_to_clauses/4, satisfiable/2]).
:- use_module('translator', [translate_axioms/4]).
:- use_module('pure/translator_pure', [translate_axioms_pure/4]).
:- use_module('fol/translator_fol', [translate_axioms_fol/4]).
:- use_module('old/translator_old', [translate_axioms_old/4]).
:- use_module('logic_unfold', [logic_unfold/2]).
:- use_module('../core/config',[get_dlog_option/2]).

% axioms_to_clauses(+SHIQAxioms,+Previous,-Clauses,-Save)
% SHIQAxioms tartalmazza a forditando axiomakat
% [CInclusion, RInclusion, Transitive] alakban
% Previous a korabbi eltarolt adatszerkezet
% Clauses az eredo klozhalmaz
% Save az eltarolando adatszerkezet
axioms_to_clauses(SHIQAxioms,_Previous,Clauses,_Save):-
	get_dlog_option(calculus,Option),
	(
	  Option = old ->
	  translate_axioms_old(SHIQAxioms,Clauses2,_Hbox,_Transitive)
	; Option = fol ->
	  translate_axioms_fol(SHIQAxioms,Clauses2,_Hbox,_Transitive)
	; Option = pure ->
	  translate_axioms_pure(SHIQAxioms,Clauses2,_Hbox,_Transitive)
	; Option = dl ->
	  translate_axioms(SHIQAxioms,Clauses2,_Hbox,_Transitive)
	),
	get_dlog_option(logic_unfold,Option2),
	(
	  Option2 = yes ->
	  logic_unfold(Clauses2,Clauses)
	; Clauses = Clauses2
	).
	

% axioms_to_clauses(+URI,+Axioms,-Clauses,-Ibox,-Hbox,-Trbox):-
% URI-val azonositott SHIQ tudasbazis Axioms, melynek
% forditasabol kapjuk Clauses klozhalmazt
% Ibox az inverzeket tartalmazza
% Hbox a szerephierarchiat
% Trbox a tranzitiv szerepeket
axioms_to_clauses(_URI, SHIQAxioms,Clauses,_Ibox,Hbox,Transitive):-

	get_dlog_option(calculus,Option),
	(
	  Option = old ->
	  translate_axioms_old(SHIQAxioms,Clauses2,Hbox,Transitive)
	; Option = fol ->
	  translate_axioms_fol(SHIQAxioms,Clauses2,Hbox,Transitive)
	; Option = pure ->
	  translate_axioms_pure(SHIQAxioms,Clauses2,Hbox,Transitive)
	; Option = dl ->
	  translate_axioms(SHIQAxioms,Clauses2,Hbox,Transitive)
	),
	get_dlog_option(logic_unfold,Option2),
	(
	  Option2 = yes ->
	  logic_unfold(Clauses2,Clauses)
	; Clauses = Clauses2
	).

% satisfiable(+Axioms, +Query):-
% Query fogalom kielegitheto Axioms T-doboz mellett
satisfiable(Axioms,Query):-
	NewAxiom = implies(top,some(arole('$newrole$'),Query)),
	axioms_to_clauses(_,[NewAxiom|Axioms],Clauses,_,_,_),
	\+ member([],Clauses).