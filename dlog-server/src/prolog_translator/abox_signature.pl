:- module(abox_signature,[abox_signature/5]).

:- use_module(library(lists)).

abox_signature(ABox, DBPredicates0, ABoxData, Signature, DBPredicates) :-
	(
	  abox_preds(ABox, ABoxData)->
	  abox_signature(ABoxData, Signature0)
	;
	  ABoxData = [], Signature0 = []
	),
	database_signature(Signature0, DBPredicates0, Signature, DBPredicates).

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
abox_axiom00(not(aconcept(Name0)), Value, Name, Value) :-
	atom_concat('not_', Name0, Name).  %TODO: _


abox_signature([], []).
abox_signature([P-L|As], [S|Ss]) :-
	(
	  L = [_-[*]|_] ->
	  S = P/1
	;
	  S = P/2
	),
	abox_signature(As, Ss).


%database_signature(Signature0, DBPreds0, Signature, DBPreds):
%add the concepts/roles defined in databases in DBPreds0 to Signature, 
%remove any incompletely defined DB predicates
database_signature(Signature, [], Signature, []).
database_signature(Signature0, [Functor-Connection-Query | DBPreds0], Signature, DBPreds) :-
	(	nonvar(Connection),
		nonvar(Query)
	->	(	memberchk(Functor, Signature0) %correct query
		->	Signature1 = Signature0 %predicate has other ABox clauses
		;	Signature1 = [Functor|Signature0] %predicate is DB only
		),
		DBPreds = [Functor-Connection-Query | DBPreds1]
	;	Signature1 = Signature0, %Connection or query missing -> ignoring
		DBPreds = DBPreds1,
		warning(kb_manager, database_signature(...) -> Functor-Connection-Query, 'Incompletely defined DB access.')
	),
	database_signature(Signature1, DBPreds0, Signature, DBPreds1).

