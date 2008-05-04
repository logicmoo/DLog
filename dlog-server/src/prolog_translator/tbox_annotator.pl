:- module(tbox_annotator,[annotated_preds/2]).

:- use_module(library(lists)).

annotated_preds([], _URI, []).
annotated_preds([Name/Arity-Clauses|Preds], _URI, [Name/Arity-AClauses|APreds]) :-
	annotated_clauses(Clauses, AClauses),
	annotated_preds(Preds, APreds).

annotated_clauses([], []).
annotated_clauses([Body-Head|Cs], [cl(Head, ABody, [Alias = [Head]])|ACs]) :-
	annotated_body(Body, Alias, ABody),
	annotated_clauses(Cs, ACs).

annotated_body([], _, []).
annotated_body([G|Gs], Alias, [goal(G, Alias)|AGs]) :-
	annotated_body(Gs, Alias, AGs).
	


	

