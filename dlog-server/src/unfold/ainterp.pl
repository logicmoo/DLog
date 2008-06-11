remove_unused_clauses(Prog0, ABoxSignature, Options, Prog, FailingPs) :-
	unzip1(Prog0, ProgPreds),
	ord_union(ABoxSignature, ProgPreds, DefPreds0),
	normalise_program(Prog0, DefPreds0, ABoxSignature, Prog1, DefPreds1, EmptyPs0),
	(   memberchk(remove_clauses_ai(off), Options) ->
	    Prog2 = Prog1, EmptyPs = EmptyPs0, FailingPNames = []
	;   called_predicates_of_program(Prog1, CalledPreds),
	    unary_functor_names(CalledPreds, CalledPNames),
	    unary_functor_names(DefPreds1, DefPNames),
	    unary_functor_names(ABoxSignature, ABoxPNames),
	    failing_preds(Prog1, DefPNames, CalledPNames, ABoxPNames, FailingPNames),
	    unused_clauses(Prog1, ABoxPNames, FailingPNames,
			   UnusedClauses, EmptyPs),
	    remove_unused_clauses1(UnusedClauses, Prog1, Prog2)
	),
	findall(Func-[], member(Func, EmptyPs), Prog, Prog2),
	unary_name_functors(FailingPNames, FailingPs).

unused_clauses(Prog, _ABoxPNames, FailingPNames, UnusedClauses, EmptyPs) :-
	exclude_prednames(Prog, FailingPNames, SuccProg),
	anc_graph_of_program(SuccProg, SuccAncGraph),
	desc_graph_of_program(Prog, _CallGraph, DescGraph),
	findall(NeededPNames,
		predicates_may_be_needed(SuccAncGraph, DescGraph, FailingPNames, NeededPNames),
		NeededPNamesL),
	ord_union(NeededPNamesL, NeededPNames),
	ord_subtract(FailingPNames, NeededPNames, ReallyFailingPNames),
	findall(Func-ClNos,
		clauses_calling_one_of_prednames(SuccProg, Func, ClNos, ReallyFailingPNames),
		UnusedClauses0),
	append(ReallyFailingPNames, UnusedClauses0, UnusedClauses),
	unary_name_functors(ReallyFailingPNames, EmptyPs).

unary_functor_names(FuncList, NameList) :-
	findall(Name, member(Name/1, FuncList), NameList).

unary_name_functors(NameList, FuncList) :-
	findall(Name/1, member(Name, NameList), FuncList).


clauses_calling_one_of_prednames(Prog, Func, ClNums, PNames) :-
	member(Func-Cls, Prog),
	findall(ClNum,
		clause_calls_one_of_prednames(Cls, PNames, ClNum),
		ClNums0
	       ),
	ClNums0 \== [],
	sort(ClNums0, ClNums).

exclude_prednames([], _, []).
exclude_prednames([Name/1-_|Prog0], ExclNames, Prog) :-
	ord_member(Name, ExclNames), !,
	exclude_prednames(Prog0, ExclNames, Prog).
exclude_prednames([FCls|Prog0], ExclNames, [FCls|Prog]) :-
	exclude_prednames(Prog0, ExclNames, Prog).

clause_calls_one_of_prednames(Cls, PNames, ClNum) :-
	nth0(ClNum, Cls, cl(_,Body,_)),
	member(goal(G,_), Body),
	functor(G, GName, 1),
	ord_member(GName, PNames).

predicates_may_be_needed(SuccAncGraph, DescGraph, FailingPNames, NeededPNames) :-
	member(PossSuccName, FailingPNames),
	edge_endpoints(PossSuccName/1, Callers, SuccAncGraph), Callers \== [],
	edge_endpoints(PossSuccName/1, Calleds, DescGraph),
	ord_union(Calleds, [PossSuccName/1], CalledOrSelfs),
	negated_functors(CalledOrSelfs, NegCalledOrSelfs),
	ord_intersect(NegCalledOrSelfs, Callers),
	unary_functor_names(CalledOrSelfs, NeededPNames).

remove_unused_clauses1([], Prog0, Prog) :-
	keysort(Prog0, Prog).
remove_unused_clauses1([UCl|UCls], Prog0, Prog) :-
	(   remove_unused_clauses2(UCl, Prog0, Prog1) -> true
	;   Prog1 = Prog0
	),
	remove_unused_clauses1(UCls, Prog1, Prog).

remove_unused_clauses2(PName, Prog0, Prog) :-
	atom(PName), !,
	select(PName/1-_, Prog0, Prog).
remove_unused_clauses2(Func-ClNums, Prog0, Prog) :-
	select(Func-Cls0, Prog0, Prog1),
	remove_list_elems(ClNums, 0, Cls0, Cls),
	Prog = [Func-Cls|Prog1].

remove_list_elems([], _, Es, Es).
remove_list_elems([N|Ns], M, [E|Es0], Es) :-
	M1 is M+1, 
	(   N == M ->
	    remove_list_elems(Ns, M1, Es0, Es)
	;   Es = [E|Es1],
	    remove_list_elems([N|Ns], M1, Es0, Es1)
	).

% normalise_program: removes clauses with undefined binary calls
%                    removes empty (possibly orphan) predicates
% TODO: Make the predicate below more efficient
normalise_program(Prog0, DefPreds0, ABoxSignature, Prog, DefPreds, EmptyPs) :-
	select(F-Cls0, Prog0, Prog1),
	select(Cl0, Cls0, Cls1),
	contains_undefined_binary(Cl0, DefPreds0), !,
	(   Cls1 == [] ->
	    Prog2 = Prog1,
	    (   ord_member(F, ABoxSignature) ->
		DefPreds1 = DefPreds0,
		EmptyPs = EmptyPs0
	    ;   select(F, DefPreds0, DefPreds1) ->
		EmptyPs = [F|EmptyPs0]
	    )
	;   Prog2 = [F-Cls1|Prog1],
	    DefPreds1 = DefPreds0,
	    EmptyPs = EmptyPs0
	),
	normalise_program(Prog2, DefPreds1, ABoxSignature, Prog, DefPreds, EmptyPs0).
normalise_program(Prog0, DefPreds0, ABoxSignature, Prog, DefPreds, EmptyPs) :-
	select(F-[], Prog0, Prog1), !,
	select(F, DefPreds0, DefPreds1),
	EmptyPs = [F|EmptyPs0],
	normalise_program(Prog1, DefPreds1, ABoxSignature, Prog, DefPreds, EmptyPs0).
normalise_program(Prog0, DefPreds0, _ABoxSignature, Prog, DefPreds0, []) :-
	keysort(Prog0, Prog).

contains_undefined_binary(cl(_,Body,_), DefPreds) :-
	member(goal(G,_), Body),
	functor(G, GName, 2),
	\+ ord_member(GName/2, DefPreds).

failing_preds(Prog0, DefPNames, CalledPNames, ABoxPNames, FailingPNames) :-
	ord_subtract(DefPNames, ABoxPNames, NonABoxPNames),
	ord_subtract(CalledPNames, DefPNames, FailingPNames0),
	assert_ainterp_program0(Prog0, ABoxPNames, NonABoxPNames),
	defined_preds_doomed_to_fail(NonABoxPNames, FailingPNames1),
	ord_union(FailingPNames0, FailingPNames1, FailingPNames).

assert_ainterp_program0(Prog0, ABoxPNames, NonABoxPNames) :-
	retractall(ainterp:acl(_,_,_)),
	aclause_in_program(Prog0, ABoxPNames, NonABoxPNames, ACl),
	assert(ainterp:ACl),
	fail.
assert_ainterp_program0(_,_,_).

aclause_in_program(Prog0, ABoxPNames, NonABoxPNames, ACl) :-
	member(Name, NonABoxPNames),
	memberchk(Name/1-Cls, Prog0),
	aclause_in_predicate(Name, Cls, ABoxPNames+NonABoxPNames, ACl).

aclause_in_predicate(Name, Cls, Ctxt, acl(Name,AL,ABody)) :-
	nonempty_aclause_bodies_in_predicate(Cls, Ctxt, ABodies0), !,
	sort(ABodies0, ABodies),
	member(ABody0, ABodies),
	add_anclist(ABody0, AL, ABody).
aclause_in_predicate(Name, _Cls, _Ctxt, acl(Name,_,[])).

add_anclist([], _, []).
add_anclist([goal(GName,Ancs)|ABody0], AL, [goal(GName,AncsAL)|ABody]) :-
	append(Ancs, AL, AncsAL),
	add_anclist(ABody0, AL, ABody).

nonempty_aclause_bodies_in_predicate([], _Ctxt, []).
nonempty_aclause_bodies_in_predicate([Cl|Cls], Ctxt, ABodies) :-
	(   clause_to_aclause_body(Cl, Ctxt, ABody) ->
	    ABody \== [],
	    ABodies = [ABody|ABodies0],
	    nonempty_aclause_bodies_in_predicate(Cls, Ctxt, ABodies0)
	;   nonempty_aclause_bodies_in_predicate(Cls, Ctxt, ABodies)
	).

clause_to_aclause_body(cl(_Head,Body,AMap), Ctxt, ABody) :-
	process_amap_empty_ancs(AMap),
	body_to_aclause_body(Body, Ctxt, ABody0),
	sort(ABody0, ABody).

body_to_aclause_body([], _Ctxt, []).
body_to_aclause_body([Goal|Goals], Ctxt, ABody) :-
	goal_to_agoal(Goal, Ctxt, ABody, ABody0),
	body_to_aclause_body(Goals, Ctxt, ABody0).

goal_to_agoal(goal(G,AncsL), Ctxt, ABody, ABody0) :-
	Ctxt = ABoxPNames+_NonABoxPNames,
	functor(G, GName, Arity),
	(   Arity == 2 -> ABody = ABody0
	;   ord_member(GName, ABoxPNames) -> ABody = ABody0
	;   % ord_member(GName, NonABoxPNames),
	    cond_to_agoals(AncsL, GName, ABody, ABody0)
	).
goal_to_agoal('$checkanc'(Anc), _, ABody, ABody0) :-
	functor(Anc, AncName),
	ABody = ['$checkanc'(AncName)|ABody0].
goal_to_agoal('$checkanc'(_,_), _, ABody, ABody0) :-
	ABody = ABody0.

cond_to_agoals([], _) --> [].
cond_to_agoals([Ancs|AncsL], G) -->
	{remove_ancs_args(Ancs, Ancs0)},
	[goal(G,Ancs0)],
	cond_to_agoals(AncsL, G).


remove_ancs_args(ATs, As) :- simple(ATs), !, As = ATs.
remove_ancs_args([AT|ATs], [A|As]) :-
	functor(AT, A, _),
	remove_ancs_args(ATs, As).

process_amap_empty_ancs([]).
process_amap_empty_ancs([AGoal|AGoals]) :-
	(   AGoal = na(AL1,AL2) -> AL2 = AL1
	;   AGoal = na(AL0,AL1,AL2) -> append(AL1, AL0, AL2)
	),
	process_amap_empty_ancs(AGoals).

defined_preds_doomed_to_fail([], []).
defined_preds_doomed_to_fail([PName|PNames0], PNames) :-
	(   predicate_can_succeed(PName) ->
	    defined_preds_doomed_to_fail(PNames0, PNames)
	;   PNames = [PName|PNames1],
	    defined_preds_doomed_to_fail(PNames0, PNames1)
	).

predicate_can_succeed(Goal) :-
	ainterp0(Goal), !.

ainterp0(G) :-
	ainterp0(goal(G,[]), [], [], []).

ainterp0([], _, _).
ainterp0([Goal|Goals], AL, LEL) :-
	ainterp0(Goal, Goals, AL, LEL).

ainterp0('$checkanc'(A), Goals, AL, LEL) :- !,
	member(A, AL),
	ainterp0(Goals, AL, LEL).
ainterp0('$checkanc'(A, B), Goals, AL, LEL) :- !,
	(   A = B -> true
	;   memberchk(A, AL)
	),
	ainterp0(Goals, AL, LEL).
ainterp0(goal(G,Ancs), Goals, AL, LEL0) :-
	ainterp_goal0(G, Ancs, LEL0),
	ainterp0(Goals, AL, LEL0).

ainterp_goal0(G, _AL, LEL0) :-
 	member(G0, LEL0), G0 == G, !,
	fail.
ainterp_goal0(GN, AL, _LEL0) :-
	negated_predname(GN, NGN),
 	memberchk(NGN, AL), !.
ainterp_goal0(G, AL, LEL0) :-
	ainterp:acl(G, AL, Body),
	ainterp0(Body, AL, [G|LEL0]).

