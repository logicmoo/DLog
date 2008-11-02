:- module(axioms_to_clauses, [axioms_to_clauses/4, satisfiable/2]).
:- use_module('translator', [translate_axioms/2]).
:- use_module('pure/translator_pure', [translate_axioms_pure/2]).
:- use_module('fol/translator_fol', [translate_axioms_fol/2]).
:- use_module('old/translator_old', [translate_axioms_old/2]).
:- use_module('logic_unfold', [logic_unfold/2]).
:- use_module('../core/config',[get_dlog_option/2]).
:- use_module(show).

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
	  translate_axioms_old(SHIQAxioms,Clauses2)
	; Option = fol ->
	  translate_axioms_fol(SHIQAxioms,Clauses2)
	; Option = pure ->
	  translate_axioms_pure(SHIQAxioms,Clauses2)
	; Option = dl ->
	  translate_axioms(SHIQAxioms,Clauses2)
	),
	get_dlog_option(logic_unfold,Option2),
	(
	  Option2 = yes ->
	  logic_unfold(Clauses2,Clauses)
	; Clauses = Clauses2
	),
	get_dlog_option(logging_detail,DebugMode),
	(
	  DebugMode = info ->
	  nl, print('Input Tbox: '), nl, show(SHIQAxioms), nl,
	  nl, print('Translated Tbox: '), nl, show(Clauses), nl
	; true
	).
	

% satisfiable(+Axioms, +Query):-
% Query fogalom kielegitheto Axioms T-doboz mellett
satisfiable(Axioms,Query):-
	NewAxiom = implies(top,some(arole('$newrole$'),Query)),
	axioms_to_clauses([NewAxiom|Axioms],_Previous,Clauses,_Save),
	\+ member([],Clauses).