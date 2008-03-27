:- module(dl_interp, [solve/3, solve/2, solve/1, solve_n/2, load_dl/1, itrace/1, assert_clauses/1, assert_clause/1]).

trace_count_pred(trace(_,_,_)).
trace_count_pred(count(_)).
trace_count_pred(count(_,_)).
trace_count_pred(trace2(_,_)).
trace_count_pred(counter_value(_,_)).
trace_count_pred(max(_,_)).
trace_count_pred(count_failure(_)).

:- use_module('../config').
:- use_module(library(lists)).
:- target(sicstus) -> 
        use_module(library(terms))
        ; true.
:- target(swi) -> 
        use_module(prolog_translator_swi_tools, [bb_put/2, bb_get/2])
        ; true.
:- use_module(transforming_tools, [contra/2, neg/2, add_key/2, unzip_second/2, list_to_open_list/2]).

user:goal_expansion(Goal, dl_interp, G) :-
	bb_get(itrace, none),
	trace_count_pred(Goal),
	G = true.

itrace(Val) :-
	(   bb_get(itrace, Old) -> true
	;   Old = off
	),
	bb_put(itrace, Val),
	(   needs_recompilation(Old, Val) ->
	    compile(interp)
	;   true
	).

needs_recompilation(none, New) :-
	!, New \== none.
needs_recompilation(_, none).

% interp(G, AL, D): A D mélységû G cél igaz, ha õseinek listája AL.
interp(true, _, _D) :- !,               	count(fact),
	true.					
interp((G1, G2), AL, D) :- !,			
    interp(G1, AL, D),				
    interp(G2, AL, D).

interp(not_eq(X,Y),AL,D):- !,
	\+ interp(eq(X,Y),AL,D),
	dif(X,Y).

interp(G, AL, D) :-				
    (   
        member(G0, AL), G0 == G ->		
						trace(D, 'Identical ancestor ~w found, failing\n', [G]),
						count(loop),
%         member(G0, AL), G0 = G ->		
% 						trace(D, 'Unifiable ancestor ~@ found, failing\n', [
% 							write_term(G, [max_depth(8)])]),
% 						count(loop),
%         member(G0, AL), subsumes(G0, G) ->		
% 						trace(D, 'More general ancestor ~@ found, failing\n', [
% 							write_term(G, [max_depth(8)])]),
% 						count(loop),
	fail					
    ;   neg(G, NG),
        member(NG, AL),
%	member(NG0, AL),			
%	unify_with_occurs_check(NG0, NG),	
%	member(NG0, AL), NG0 == NG,     	% solve(q3) fails with this variant!
						count(ancres),
						trace(D, 'Negated ancestor ~w found, succeeding\n', [G]),
	true					
%	true -> true                    	% solve(X^ \komoly(X)) fails with this variant!
    ;                                   	trace2(trace(D, 'Entering goal ~w\n', [G]),
	                                	       trace(D, 'Goal ~w failed\n', [G])),
                                		counter_value(choice, ChCount),
	D1 is D+1,				
	AL1 = [G|AL], 				
	(   ground(G) ->			
	    dl_clause(G, B),			
						    max(depth, D1),
						    count(gcall),
	    interp(B, AL1, D1),			
	    !,					
	                                	    trace(D, 'Ground goal successful ~w\n', [G]),
	    true				
	;                               	    count(choice, -1),
	    dl_clause(G, B),			
	                                	    count(choice),
						    max(depth, D1),
						    count(call),
	    interp(B, AL1, D1),			
						    trace2(trace(D, 'Goal successful ~w\n', [G]),
							   trace(D, 'Redoing goal ~w\n', [G])),
	    true				
	;                               	    count_failure(ChCount), 
	    fail
	)
    ).

dl_clause(G, B) :-
	clause(dl:G, B).

% dl_clause(G, B) :-
% 	functor(G, F, N),
% 	functor(G0, F, N),
% 	clause(dl:G0, B),
% 	unify_with_occurs_check(G0, G).

count_failure(ChCount) :-
	counter_value(choice, ChCount1),
	ChCount1 =:= ChCount-1, !,
	count(choice), count(fail).
count_failure(_).

count(C) :-
	count(C, 1).

count(C, N) :-
	bb_get(C, I),
	I1 is I+N,
	bb_put(C, I1).

max(C, N) :-
	bb_get(C, I),
	I1 is max(I,N),
	bb_put(C, I1).

counter(fact).
counter(loop).
counter(ancres).
counter(fail).
counter(choice).
counter(depth).
counter(call).
counter(gcall).
counter(solnum).


init_counts :-
	counter(C), bb_put(C, 0), fail.
init_counts.

counter_value(C, V) :-
	counter_value0(C, V).

counter_value0(C, V) :-
	counter(C), bb_get(C, V).

get_counts(CVs) :-
	findall(C = V, counter_value0(C, V), CVs).

trace2(G1, G2) :-
	bb_get(itrace, on(_)), !,
	(   G1
	;   G2, fail
	).
trace2(_, _).

trace(D, F, DL) :-
	bb_get(itrace, on(Int)), !,
	trace0(D, F, DL),
	(   Int = int -> get0(_)
	;   true
	).
trace(_, _, _).

trace0(D, F, DL) :-
	format('~N', []),
	Indent is D*2,
	tab(Indent),
	format(F, DL).

solve(Q, X, T) :-
	statistics(runtime, [T0,_]),
	solve_base(Q, X),
	statistics(runtime, [T1,_]),
	T is T1-T0.

solve_base(Q, X) :-
	init_uniquify,
	init_counts,
	make_goal(Q, X, Goal, AL),
	interp(Goal, AL, 0),	
	uniquify(X),
	count(solnum).


make_goal((AL0:Q), X, Goal, AL) :- 
	AL0 = [_|_], !, AL = AL0,
	make_goal(Q, X, Goal, _).
make_goal(Y^Q, X, Goal, AL) :- !,
	X= Y, Goal = Q, AL=[].
make_goal(Q, X, Goal, []) :-
	Goal =.. [Q,X].

solve(Q) :-
	solve(Q, _X).

solve(Q, X) :-
	statistics(runtime, [T0,_]),
	(   solve_base(Q, X),
	    % statistics(runtime, [T1,_]),
	    % T is T1-T0,	  
	    % get_counts(CVs),
	    % format('solution: ~w, ~t~20|~t~3d sec~31|, ~w~n', [X, T, CVs]),
	    fail
	;   statistics(runtime, [T1,_]),
	    T is T1-T0,
	    get_counts(CVs),
	    format('no more solutions, ~t~20|~t~3d sec~31|, ~w~n', [T,CVs])
	).


% solve_n(+Vars,+Goal):
% ekvivalens solve(Vars^Goal) hivasaval, de Goal-ban eloreveszi a
% ketargumentumu celokat
%  keszitette: Borosan Peter (BME-SZIT)
solve_n(Vars,Goal):-
	open_list_to_list(Goal,GoalL),
	add_key(GoalL, GoalK),
	keysort(GoalK, AGoal),
	unzip_second(AGoal, NewGoalL),
	list_to_open_list(NewGoalL,NewGoal),
	solve(Vars^NewGoal).
	

:- dynamic found/1.
init_uniquify :-
	retractall(found(_)).

uniquify(X) :-
	\+ found(X), assertz(found(X)).


load_dl(File) :-
	prolog_flag(discontiguous_warnings, DWF),
	set_prolog_flag(discontiguous_warnings, off),
	call_cleanup(load_files(dl:File, [compilation_mode(assert_all)]),
		     set_prolog_flag(discontiguous_warnings, DWF)
		    ).

:- op(1200, xfx, :--).


% assert_clauses(+Clauselist):
% Clauselit negalt literalokbol allo klozok listaja
% eloallitjuk az osszes hozzajuk tartozo kontrapozitivot es asszertaljuk oket
assert_clauses([]).
assert_clauses([C|Cs]):-
	assert_clause(C),
	assert_clauses(Cs).
	

% assert_clause(+LitList0):
% LitList0 negalt literal listanak megfelelo kontrapozitivok assertalasa
assert_clause(LitList0):-
	contra(LitList0, Contras),
	assert_contrapositives(Contras).

assert_contrapositives([]).
assert_contrapositives([Cl|Cls]):-
	( retract(dl:Cl), fail
	; true
	),
	assert(dl:Cl),
	assert_contrapositives(Cls).


clause_to_litlist(Head, Body, [NH|BL]) :-
	neg(Head, NH),
	open_list_to_list(Body, BL).


open_list_to_list(true, L) :- !, L = [].
open_list_to_list((G0,G1), L) :-
	!, L = [G0|L1],
	open_list_to_list(G1, L1).
open_list_to_list(G0, [G0]).