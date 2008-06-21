:- module(axioms_to_clauses, [axioms_to_clauses/6]).
:- use_module('translator', [translate_axioms/4]).
:- use_module('fol/translator_fol', [translate_axioms_fol/4]).
:- use_module('old/translator_old', [translate_axioms_old/4]).
:- use_module('logic_unfold', [logic_unfold/2]).
:- use_module('../dlog',[get_dlog_option/2]).


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
	; Option = dl ->
	  translate_axioms(SHIQAxioms,Clauses2,Hbox,Transitive)
	),
	get_dlog_option(logic_unfold,Option2),
	(
	  Option2 = yes ->
	  logic_unfold(Clauses2,Clauses)
	; Clauses = Clauses2
	).