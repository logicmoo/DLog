:- module(abox_signature,[abox_signature/4]).

:- use_module(library(lists)).

abox_signature(ABox, DBPredicates, ABoxData, Signature) :-
	(
	  abox_preds(ABox, ABoxData)->
	  abox_signature(ABoxData, Signature0)
	;
	  ABoxData = [], Signature0 = []
	),
	database_signature(DBPredicates, Signature0, Signature).

% SzP: Ha az abox_axiom(ABox, P, A, B) nem adhat vissza ket azonso <P,A,B>
% harmast, akkor itt egy kicsit gyorsitani lehet azzal, hogy setof helyett
% bagof-ot irunk itt; felteve persze hogy a dolgok abc-rendezese nem fontos...
abox_preds(ABox, ABoxStr) :-
	bagof(P-AhBss, setof(A-Bs, setof(B, abox_axiom(ABox, P, A, B), Bs), AhBss), ABoxStr).

abox_axiom(ABox, Name, Value1, Value2) :-
	member(C, ABox),
	abox_axiom0(C, Name, Value1, Value2).

abox_axiom0(cassertion(N, V), Name, Value, *) :-
	abox_axiom00(N, V, Name, Value).
abox_axiom0(rassertion(arole(Name), Value1, Value2), Name, Value1, Value2).

abox_axiom00(aconcept(Name), Value, Name, Value).
abox_axiom00(not(aconcept(Name)), Value, not(Name), Value).


abox_signature([], []).
abox_signature([P-L|As], [S|Ss]) :-
	(
	  L = [_-[*]|_] ->
	  S = P/1
	;
	  S = P/2
	),
	abox_signature(As, Ss).


%database_signature(DBPreds, Signature0, Signature):
%add the concepts/roles defined in databases in DBPreds0 to Signature, 
database_signature([], Signature, Signature).
database_signature([access(Functor, _Connection, _Access) | DBPreds], Signature0, Signature) :-
	(	memberchk(Functor, Signature0)
	->	Signature1 = Signature0 %predicate has other ABox clauses
	;	Signature1 = [Functor|Signature0] %predicate is DB only
	),
	database_signature(DBPreds, Signature1, Signature).

