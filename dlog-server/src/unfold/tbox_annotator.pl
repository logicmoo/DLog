:- module(tbox_annotator,[annotated_preds/2,body_to_list/2]).

:- use_module(library(lists)).

annotated_preds([], []).
annotated_preds([Name/Arity-Clauses|Preds], [Name/Arity-AClauses|APreds]) :-
	annotated_clauses(Clauses, AClauses),
	annotated_preds(Preds, APreds).

annotated_clauses([], []).
annotated_clauses([Body-Head|Cs], [cl(Head, ABody, [na([Head],Alias)])|ACs]) :-
	annotated_body(Body, Alias, ABody),
	annotated_clauses(Cs, ACs).

annotated_body(Body, Alias, ABody) :-
	body_to_list(Body, BodyList),
	annotated_bodylist(BodyList, Alias, ABody).

annotated_bodylist([], _, []).
annotated_bodylist([G|Gs], Alias, [goal(G,[Alias])|AGs]) :-
	annotated_bodylist(Gs, Alias, AGs).
	
body_to_list(true, []) :- !.
body_to_list((G1,G2), [G1,Gx|Gs]) :-   % at-least two goals
	!, body_to_list(G2, [Gx|Gs]).
body_to_list(G, [G]).


	

