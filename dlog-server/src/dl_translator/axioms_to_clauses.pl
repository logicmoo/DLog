:- module(axioms_to_clauses, [axioms_to_clauses/6]).
:- use_module('translator', [translate_axioms/4]).
:- use_module('fol/translator_fol', [translate_axioms_fol/4]).
:- use_module('../config',[get_dlog_option/2]).


% axioms_to_clauses(+URI,+Axioms,-Clauses,-Ibox,-Hbox,-Trbox):-
% URI-val azonositott SHIQ tudasbazis Axioms, melynek
% forditasabol kapjuk Clauses klozhalmazt
% Ibox az inverzeket tartalmazza
% Hbox a szerephierarchiat
% Trbox a tranzitiv szerepeket
axioms_to_clauses(_URI, SHIQAxioms,Clauses,_Ibox,Hbox,Transitive):-

	get_dlog_option(dl_calculus,Option),
	(
	  Option = no ->
	  translate_axioms_fol(SHIQAxioms,Clauses,Hbox,Transitive)
	; translate_axioms(SHIQAxioms,Clauses,Hbox,Transitive)
	).



	% elhagyjuk a tranzitivitashoz tartozo klozokat
%	findall(C,(
%		   member(C,Clauses1),
%		   \+ (
%			contains_struct2(C,nconcept(Pred,_)),
%			atom_concat('trans',_,Pred)
%		      ),
%		  ), Clauses	       
%	       ).