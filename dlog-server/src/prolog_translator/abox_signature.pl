:- module(abox_signature,[abox_signature/3]).

:- use_module(library(lists)).

abox_signature(ABox, ABoxStr, Signature) :-
	(
	  abox_preds(ABox, ABoxStr)->
	  abox_signature(ABoxStr, Signature)
	;
	  ABoxStr = [], Signature = []
	).

% SzP: Ha az abox_axiom(ABox, P, A, B) nem adhat vissza ket azonso <P,A,B>
% harmast, akkor itt egy kicsit gyorsitani lehet azzal, hogy setof helyett
% bagof-ot irunk itt; felteve persze hogy a dolgok abc-rendezese nem fontos...
abox_preds(ABox, ABoxStr) :-
	bagof(P-AhBss, setof(A-Bs, setof(B, abox_axiom(ABox, P, A, B), Bs), AhBss), ABoxStr).

abox_axiom(ABox, Name, Value1, Value2) :-
	member(C, ABox),
	abox_axiom0(C, Name, Value1, Value2).

abox_axiom0(aconcept(Name, Value), Name, Value, *).
abox_axiom0(arole(Name, Value1, Value2), Name, Value1, Value2).
abox_axiom0(not(aconcept(Name0, Value)), Name, Value, *) :-
	atom_concat('not_', Name0, Name).


abox_signature([], []).
abox_signature([P-L|As], [S|Ss]) :-
	(
	  L = [_-[*]|_] ->
	  S = P/1
	;
	  S = P/2
	),
	abox_signature(As, Ss).