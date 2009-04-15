:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(library(ordsets)).

unfold_predicates(InputProg, UnfoldedProg) :-
	unfold_predicates0(InputProg, [], uprog(UnfoldedProg,_)).

unfold_predicates0(InputProg, Options, uprog(UProgram,PredInfo)) :-
	unfold_predicates1(InputProg, [recdet(off)|Options], uprog(UProgram0,PredInfo)),
	deannotate_gprog(UProgram0, UProgram).

unfold_predicates(InputProg, Options, UnfoldedProg) :-
	memberchk(compat(on), Options), !,
	unfold_predicates0(InputProg, Options, UnfoldedProg).
unfold_predicates(InputProg, Options, UnfoldedProg) :-
	unfold_predicates1(InputProg, Options, UnfoldedProg).


deannotate_gprog([], []).
deannotate_gprog([F-Cls0|Prog0], [F-Cls|Prog]) :-
	deannotate_clauses(Cls0, Cls),
	deannotate_gprog(Prog0, Prog).

deannotate_clauses([], []).
deannotate_clauses([Cl0|Cls0], [Cl|Cls]) :-
	deannotate_clause(Cl0, Cl),
	deannotate_clauses(Cls0, Cls).

deannotate_clause(cl(Head,Body0,AMap), Body-Head) :-
	process_amap(AMap, []),
	deannotate_goals(Body0, Head, Body1),
	body_to_list(Body, Body1).

deannotate_goals([], _, []).
deannotate_goals([G0|Gs0], Head, [G|Gs]) :-
	deannotate_goal(G0, Head, G),
	deannotate_goals(Gs0, Head, Gs).

deannotate_goal('$checkanc'(A), _Head, Goal) :-
	negated_goal_pattern(A, Goal, _).
deannotate_goal('$checkanc'(A,B), Head, Goal) :- 
	B == Head, 
	negated_goal_pattern(A, Goal, _).
deannotate_goal(goal(G,AncsL), Head, G) :-
	(   ground(AncsL) -> true
	;   AncsL = [[Anc]], Anc == Head
	).	

unfold_predicates1(InputProg, Options, UnfoldedProg) :-
	InputProg = prog(Program0,ABoxSignature0,EntryPredSpec),
	UnfoldedProg = uprog(UProgram,PredInfo),
	keysort(Program0, Program1),
	entry_pred_spec(EntryPredSpec, Program1, EntryPs0),
	sort(ABoxSignature0, ABoxSignature1),
	remove_unused_clauses(Program1, ABoxSignature1, Options, Program2, FailingPs),
	treat_binaries_as_abox_preds(Program2, ABoxSignature1, Program, ABoxSignature, BinPreds),
	ord_subtract(EntryPs0, FailingPs, EntryPs1),
	anc_dependencies_of_program(Program, AncDeps),
	program_to_dprogram(Program, AncDeps, ABoxSignature, DProgram0),
	list_to_dict(DProgram0, Dictionary),
	order_clause_bodies_program(DProgram0, Dictionary, DProgram1),
	unfold_entry_preds(DProgram1, EntryPs1, Options, UProgram0, _CQPs),
	mark_recursive_predicates(UProgram0, UProgram1, Options, RecursivePs, DescGraph),
	include_entry_preds(UProgram1, EntryPs0, UProgram2),
	restore_binaries(BinPreds, UProgram2, UProgram3),
	sort(UProgram3, UProgram4),
	calculate_predinfo(UProgram4, ABoxSignature1, EntryPs0, DescGraph, RecursivePs, QPs, PredInfo),
	simplify_query_preds(UProgram4, QPs, UProgram).

treat_binaries_as_abox_preds(Program0, ABoxSignature0, Program, ABoxSignature, BinPreds) :-
	remove_binaries(Program0, Program, BinPreds),
	unzip1(BinPreds, BinPs),
	ord_union(ABoxSignature0, BinPs, ABoxSignature).

remove_binaries([], [], []).
remove_binaries([Pred0|Preds0], Preds, BPreds) :-
	(   Pred0 = _/2-_ -> BPreds = [Pred0|BPreds1],
	    remove_binaries(Preds0, Preds, BPreds1)
	;   Preds = [Pred0|Preds1],
	    remove_binaries(Preds0, Preds1, BPreds)
	).

restore_binaries([], Prog, Prog).
restore_binaries([BPred|BPreds], [Pred0|Preds0], [Pred|Preds]) :-
	(   same_pred_func(BPred, Pred0) ->
	    Pred = BPred,
	    restore_binaries(BPreds, Preds0, Preds)
	;   Pred = Pred0,
	    restore_binaries([BPred|BPreds], Preds0, Preds)
	).

same_pred_func(Func-_, Func-_).

entry_pred_spec(EntryPredSpec, Program, EntryPs) :-
	entry_pred_spec1(EntryPredSpec, Program, EntryPs0),
	sort(EntryPs0, EntryPs).

entry_pred_spec1(all, Program, EntryPs) :-
	!, findall(Func, func_occuring_in(Program, Func, any), EntryPs).
entry_pred_spec1(pos, Program, EntryPs) :-
	!, findall(Func, func_occuring_in(Program, Func, pos), EntryPs).
entry_pred_spec1(EntryPs0, EntryPs0).

func_occuring_in(Program, Func, Constraint) :-
	func_occuring_in(Program, Func0),
	(   Func = Func0
	;   Func0 = _/1, negated_functor(Func0, Func)
	),
	(   Constraint == pos ->
	    Func = Name/_,
	    \+ atom_concat('$not_', _, Name)
	;   true
	).

list_entry_only_predicates([], _, []).
list_entry_only_predicates([F|Fs], DescGraph, EOFs) :-
	(   member(_-DescFs, DescGraph), ord_member(F, DescFs) ->
	    list_entry_only_predicates(Fs, DescGraph, EOFs)
	;   EOFs = [F|EOFs1],
	    list_entry_only_predicates(Fs, DescGraph, EOFs1)
	).
	

func_occuring_in(Program, Func) :-
	member(Func-_, Program).
func_occuring_in(Program, Func) :-
	func_calls_func(_, Program, Func).

include_entry_preds([], EntryPs, Prog) :-
	findall(Func-[], member(Func, EntryPs), Prog).
include_entry_preds([Func-Cls|Prog0], EntryPs0, Prog) :-
	select(Func, EntryPs0, EntryPs), !,
	Prog = [Func-Cls|Prog1],
	include_entry_preds(Prog0, EntryPs, Prog1).
include_entry_preds([_Func-[]|Prog0], EntryPs, Prog) :-
	!, include_entry_preds(Prog0, EntryPs, Prog).
include_entry_preds([FuncCls|Prog0], EntryPs, [FuncCls|Prog]) :-
	include_entry_preds(Prog0, EntryPs, Prog).

simplify_query_preds([], _QPs, []).
simplify_query_preds([Func-Cls0|UProg0], [Func|QPs], UProg) :- !,
	UProg = [Func-Cls|UProg1],
	simplify_query_pred(Cls0, Cls),
	simplify_query_preds(UProg0, QPs, UProg1).
simplify_query_preds([Func-Cls0|UProg0], QPs, [Func-Cls|UProg]) :-
	simplify_nonquery_pred(Cls0, QPs, Cls),
	simplify_query_preds(UProg0, QPs, UProg).

simplify_query_pred([], []).
simplify_query_pred([cl(Head,Body0,_AMap)|Cls0], [cl(Head,Body,[])|Cls]) :-
	simplify_query_pred_clause_body(Body0, Body),
	simplify_query_pred(Cls0, Cls).

simplify_query_pred_clause_body([], []).
simplify_query_pred_clause_body([goal(G,AliasL0)|Body0], [goal(G,AliasL)|Body]) :-
	(   AliasL0 = [_|_] -> AliasL = query
	;   AliasL = AliasL0
	),
	simplify_query_pred_clause_body(Body0, Body).
	
simplify_nonquery_pred([], _, []).
simplify_nonquery_pred([cl(Head,Body0,AMap)|Cls0], QPs, [cl(Head,Body,AMap)|Cls]) :-
	simplify_nonquery_pred_clause_body(Body0, QPs, Body),
	simplify_nonquery_pred(Cls0, QPs, Cls).

simplify_nonquery_pred_clause_body([], _, []).
simplify_nonquery_pred_clause_body([Goal0|Body0], QPs, [Goal|Body]) :-
	simplify_nonquery_pred_goal(Goal0, QPs, Goal),
	simplify_nonquery_pred_clause_body(Body0, QPs, Body).

simplify_nonquery_pred_goal(goal(G,AliasL0), QPs, Goal) :- !,
	Goal = goal(G,AliasL),
	(   AliasL0 = [_|_],  functor(G, Name, Arity),
	    ord_member(Name/Arity, QPs)	-> AliasL = query
	;   AliasL = AliasL0
	).
simplify_nonquery_pred_goal(Goal, _QPs, Goal).

	
calculate_predinfo(Prog, ABoxSignature, EntryPs0, DescGraph, RecPs, QPs,
		   info(QPs, AtomicPs, ANRPs, RecPs, EntryPs0, EntryOnlyPs)) :-
	list_entry_only_predicates(EntryPs0, DescGraph, EntryOnlyPs0),
	findall(Func, member(Func-[], Prog), NoTBoxPreds0),
	sort(NoTBoxPreds0, NoTBoxPreds),
	ord_subtract(NoTBoxPreds, ABoxSignature, NoClausePs),
	anr_preds_of_gprog(Prog, ANRPs0),
	ord_intersection(NoClausePs, ANRPs0, OrphanPs),
	ord_subtract(ANRPs0, OrphanPs, ANRPs),
%	ord_subtract(NoClausePs, ANRPs, EmptyPs1),
	ord_intersection(NoTBoxPreds, ABoxSignature, AtomicOrSemiOrphan),
	ord_subtract(AtomicOrSemiOrphan, OrphanPs, AtomicPs),
	query_predicates_of_gprog(Prog, ANRPs, RecPs, DescGraph, QPs0),
	ord_subtract(QPs0, NoClausePs, QPs1),
	ord_subtract(QPs1, AtomicPs, QPs),
	ord_subtract(EntryOnlyPs0, NoClausePs, EntryOnlyPs).

unfold_entry_preds(DProgram0, EntryPs, Options, Program, CQPs) :-
	list_to_dict(DProgram0, Dict0),
	unfold_preds(DProgram0, env(Dict0,Options,_), DProgram1),
	recursive_predicates_of_tree_program(DProgram1, _DescGraph, RPreds),
	list_to_dict(DProgram1, Dict1),
	(   memberchk(unfold(inner), Options) ->
	    Ctxt = ctxt(single,[],[]),
	    tree_program_to_clause_program(DProgram1, Ctxt, Dict1, Program, []),
	    CQPs = []  % TODO
	;   specialise_entry_preds(DProgram1, DProgram2, PSs),
	    initial_query_pred_properties(RPreds, CQPs0),
	    handle_query_preds(DProgram2, CQPs0, PSs, Dict1, CQPs1),
	    Ctxt1 = ctxt(entry(EntryPs),CQPs1,[]),
	    tree_program_to_clause_program(DProgram2, Ctxt1, Dict1, Program, Program1),
	    (   memberchk(unfold(entry), Options) ->
		unfold_preds(DProgram0, env(Dict0,[primary(none)|Options],_), DProgram3),
		list_to_dict(DProgram3, Dict3),
		filter_query_preds_entry_only_mode(CQPs1, Dict3, CQPs)
	    ;   DProgram3 = DProgram1, Dict3 = Dict1, CQPs = CQPs1
	    ),
	    tree_program_inner_preds(DProgram3, CQPs, Dict3, Program, Program1)
	).

tree_program_inner_preds(DProgram1, CQPs, Dict, Program, Program0) :-
	collect_inner_preds(Program, InnerPreds), InnerPreds \== [], !,
	Ctxt = ctxt(inner(InnerPreds),CQPs,[]),
	tree_program_to_clause_program(DProgram1, Ctxt, Dict, Program0, Program1),
	tree_program_inner_preds(DProgram1, CQPs, Dict, Program, Program1).
tree_program_inner_preds(_, _, _, _, []).


%---------------------------------------------------------------------------
% Marking recursive predicates

mark_recursive_predicates(Program0, Program, Options, RecursivePs, DescGraph) :-
	recursive_predicates_of_program(Program0, DescGraph, RecInfo),
	(   memberchk(recdet(off), Options) -> Program = Program0
	;   mark_rec_in_preds(Program0, RecInfo, Program)
	),
	unzip1(RecInfo, RecursivePs).

mark_rec_in_preds([], _, []).
mark_rec_in_preds([Func-Cls0|Preds0], RecInfo, [Func-Cls|Preds]) :-
	mark_rec_in_clauses(Cls0, RecInfo, Cls),
	mark_rec_in_preds(Preds0, RecInfo, Preds).

mark_rec_in_clauses([], _RecInfo, []).
mark_rec_in_clauses([Cl0|Cls0], RecInfo, [Cl|Cls]) :-
	mark_rec_in_clause(Cl0, RecInfo, Cl),
	mark_rec_in_clauses(Cls0, RecInfo, Cls).

mark_rec_in_clause(cl(Head,BGoals0,AMap), RecInfo, cl(Head,BGoals,AMap)) :-
	functor(Head, Name, Arity),
	(   memberchk(Name/Arity-RecSuccs, RecInfo) ->
	    PredRecInfo = rec(Head,RecSuccs)
	;   PredRecInfo = nonrec
	),
	mark_rec_in_goals(BGoals0, PredRecInfo, BGoals).

mark_rec_in_goals([], _, []).
mark_rec_in_goals([Goal0|Goals0], PredRecInfo, [Goal|Goals]) :-
	mark_rec_in_goal(PredRecInfo, Goal0, Goal),
	mark_rec_in_goals(Goals0, PredRecInfo, Goals).

mark_rec_in_goal(rec(Head,RecSuccs), goal(G,AncsL), goal(G,rec(Head,AncsL))) :-
	functor(G, Name, 1),
	\+atom(AncsL),
	memberchk(Name/1, RecSuccs), !.
mark_rec_in_goal(_, G0, G0).

%---------------------------------------------------------------------------
% A primitive implementation of a dictionary

dict_member(Entry, Dict) :-
	member(Entry, Dict).

dict_memberchk(Entry, Dict) :-
	memberchk(Entry, Dict).

list_to_dict(EntryList, Dict) :-
	Dict = EntryList.

%---------------------------------------------------------------------------
% Extracting the call graph and ancestor dependencies from the program

dependency_graphs_of_program(Prog, CallGraph, AncGraph, DescGraph) :-
	desc_graph_of_program(Prog, CallGraph, DescGraph),
	transpose_ugraph(DescGraph, AncGraph).

desc_graph_of_program(Prog, CallGraph, DescGraph) :-
	findall(Func-CalledFunc,
		func_calls_func(Func, Prog, CalledFunc),
		CallEdges),
	transitive_closure(CallEdges, CallGraph, DescGraph).

anc_graph_of_program(Prog, AncGraph) :-
	findall(CalledFunc-Func,
		func_calls_func(Func, Prog, CalledFunc),
		ParentEdges),
	transitive_closure(ParentEdges, _, AncGraph).

anc_dependencies_of_program(Prog, AncDeps) :-
	dependency_graphs_of_program(Prog, CallGraph, AncGraph, DescGraph),
	findall(N-A, nd_edge(A, N, CallGraph), CallAncEdges),
	vertices_edges_to_ugraph([], CallAncEdges, CallAncGraph),
	anc_dependencies(DescGraph, CallAncGraph, [], AncGraph, AncDeps).

called_predicates_of_program(Preds, CalledFuncs) :-
	findall(CalledFunc,
		func_calls_func(_Func, Preds, CalledFunc),
		CalledFuncs0),
	sort(CalledFuncs0, CalledFuncs).

recursive_predicates_of_program(Preds, DescGraph, RPreds) :-
	findall(Func-CalledFunc,
		func_calls_func(Func, Preds, CalledFunc),
		CallEdges),
	transitive_closure(CallEdges, CallGraph, DescGraph),
	findall(Func-CallFsRec, calls_itself_through(Func, CallGraph, DescGraph, CallFsRec), RPreds).

transitive_closure(Edges, Graph, Closure) :-
	vertices_edges_to_ugraph([], Edges, Graph),
	transitive_closure(Graph, Closure).

anc_dependencies([], _CallAncGraph, _CheckAncGraph, _AncGraph, []).
anc_dependencies([F-LDF0|DescGraph], CallAncGraph, CheckAncGraph, AncGraph, [F-LAdepFs|ADeps0]) :-
	findall(DescAncs, can_depend_on_ancs(F, LDF0, CheckAncGraph, DescAncs), DescAncDepsL),
	ord_union(DescAncDepsL, LNDF),
	edge_endpoints(F, LACallF, AncGraph),
	findall(CallAncs, can_be_invoked_with_ancs(F, LACallF, CallAncGraph, CallAncs), CallAncsL),
	ord_union(CallAncsL, LAF),
	ord_intersection(LAF, LNDF, LAdepFs),
	anc_dependencies(DescGraph, CallAncGraph, CheckAncGraph, AncGraph, ADeps0).

can_depend_on_ancs(F, LDF0, _CheckAncGraph, DescAncDeps) :-
	negated_functors([F|LDF0], LNDF0),
	sort(LNDF0, DescAncDeps).
can_depend_on_ancs(F, LDF0, CheckAncGraph, DescAncDeps) :-
	(  Func = F
	;  member(Func, LDF0)
	),
	edge_endpoints(Func, DescAncDeps, CheckAncGraph). 

can_be_invoked_with_ancs(F, LACallF, CallAncGraph, CallAncs) :-
	(  Func = F
	;  member(Func, LACallF)
	),
	edge_endpoints(Func, CallAncs, CallAncGraph).

negated_functors([], []).
negated_functors([F|Fs], [NF|NFs]) :-
	negated_functor(F, NF),
	negated_functors(Fs, NFs).
		  
func_calls_func(CallerF, Prog, CalledF) :-
	member(CallerF-Cls, Prog),
	member(cl(_,Body,_), Cls),
	member(goal(G,Ancs), Body),
	Ancs \== abox,
	functor(G, Name, Arity),
	CalledF = Name/Arity.

calls_itself_through(Node, CallGraph, DescGraph, CallFsRec) :-
	member(Node-SuccNodes, CallGraph),
	findall(SNode,
		successor_node_from_which_i_am_reachable(Node, SuccNodes, DescGraph, SNode),
		CallFsRec
	       ),
	CallFsRec \== [].

successor_node_from_which_i_am_reachable(Node, SuccNodes, DescGraph, SNode) :-
	member(SNode, SuccNodes),
	edge(SNode, Node, DescGraph).


%---------------------------------------------------------------------------
% Extracting call graph and ancestor dependencies  from a tree program

recursive_predicates_of_tree_program(TPreds, DescGraph, RPreds) :-
	findall(Func-CalledFunc,
		tree_func_calls_func(Func, TPreds, CalledFunc),
		CallEdges),
	transitive_closure(CallEdges, CallGraph, DescGraph),
	findall(Func-CallFsRec, calls_itself_through(Func, CallGraph, DescGraph, CallFsRec), RPreds).

tree_func_calls_func(CallerF, Prog, CalledF) :-
	member(pr(CallerF,Cls,_), Prog),
	goal_in_predicate_tree(Cls, Goal),
	goal_to_func_ancs(Goal, CalledF, AncsL),
	CalledF \= vareq(_)/_,
	AncsL \== abox.
	
tree_func_checks_anc(CallerF, Prog, AncName/Arity) :-
	member(pr(CallerF,Cls,_), Prog),
	goal_in_predicate_tree(Cls, Goal),
	goal_to_func_ancs(Goal, '$checkanc'/1, AncsL),
	member(Ancs, AncsL),
	member(Anc, Ancs),
	functor(Anc, AncName, Arity).

ancres_dependency_graph_of_tree_program(Prog, AncDeps) :-
	findall(Func-CalledFunc,
 		tree_func_calls_func(Func, Prog, CalledFunc),
		CallEdges),
	transitive_closure(CallEdges, _CallGraph, DescGraph),
	transpose_ugraph(DescGraph, AncGraph),
	findall(CallF-AncF,
		tree_func_is_on_anclist_of_func(AncF, Prog, CallF),
		CallAncEdges
	       ),
	vertices_edges_to_ugraph([], CallAncEdges, CallAncGraph),
	findall(PredF-AncF,
		tree_func_checks_anc(PredF, Prog, AncF),
		CheckAncEdges
	       ),
	vertices_edges_to_ugraph([], CheckAncEdges, CheckAncGraph),
	anc_dependencies(DescGraph, CallAncGraph, CheckAncGraph, AncGraph, AncDeps).

tree_func_is_on_anclist_of_func(AncName/AncArity, Prog, CalledF) :-
	member(pr(_,Cls,_), Prog),
	goal_in_predicate_tree(Cls, Goal),
	goal_to_func_ancs(Goal, CalledF, AncsL),
	AncsL \== abox,
	member(Ancs, AncsL),
	member(Anc, Ancs),
	functor(Anc, AncName, AncArity).


%---------------------------------------------------------------------------
% Determining the subset of the predicates of the original program which
% need to be added to the unfolded program.

collect_inner_preds(Program, InnerPreds) :-
	findall(Func, inner_pred(Program, Func), InnerPreds0),
	sort(InnerPreds0, InnerPreds).

inner_pred(Program, Func) :-
	(   length(Program,_) -> true
	),			% to close the open-ended list
	unzip1(Program, DefPreds0),
	sort(DefPreds0, DefPreds),
	Arity = 1,
	member(_F-Pred, Program),
	member(cl(_Head, Body, _AMap), Pred),
	member(goal(G,Alias), Body),
	(   Alias = [_|_] -> true
	;   Alias = []
	),
	functor(G, IName, Arity),
	\+ ord_member(IName/Arity, DefPreds),
	opt_deinternalise_name(IName, Name),
	Func = Name/Arity.

%---------------------------------------------------------------------------
% Code needed for predicate classification of the generated program (gprog)

neg_anc_in_gprog(Program, NName/Arity) :-
	member(_F-Pred, Program),
	member(cl(_Head, _Body, AMap), Pred),
	(   /* member(goal('$checkanc',AncG), Body)
	;   */ member(na(AncGs,_), AMap), member(AncG, AncGs)
	;   member(na(_,AncGs,_), AMap), member(AncG, AncGs)
	),
	functor(AncG, Name, Arity),
	negated_predname(Name, NName).


calls_checkanc_in_gprog(Program, F) :-
	member(F-Pred, Program),
	member(cl(_Head, Body, _AMap), Pred),
	member(Goal, Body),
	functor(Goal, '$checkanc', _).

anr_preds_of_gprog(Program, ANRFuncs) :-
	findall(AncF, neg_anc_in_gprog(Program, AncF), ANRFuncs0),
	sort(ANRFuncs0, ANRFuncs1),
	unzip1(Program, AllPreds),
	ord_intersection(ANRFuncs1, AllPreds, ANRFuncs).

query_predicates_of_gprog(Program, ANRPs, RecPs, DescGraph, QPs) :-
	findall(Func, calls_checkanc_in_gprog(Program, Func), ChAPs0),
	sort(ChAPs0, ChAPs),
	ord_union(ChAPs, ANRPs, NonQPs0),
	ord_union(NonQPs0, RecPs, NonQPs),
	findall(Func, query_predicate_of_gprog(Program, NonQPs, DescGraph, Func), QPs).

query_predicate_of_gprog(Program, NonQPs, DescGraph, Func) :-
	member(Func-_, Program),
	(   ord_member(Func, NonQPs) -> fail
	;   edge_endpoints(Func, DescFuncs, DescGraph),
	    ord_intersect(DescFuncs, NonQPs) -> fail
	;   true
	).	    
	
% query_predicates_of_gprog([], [], []).
% query_predicates_of_gprog([Func-PSs|CQPs], UncondQPs, EntryQPs) :-
% 	Func = Name/Arity,
% 	(   Arity == 2 ->
% 	    query_predicates_of_gprog(CQPs, UncondQPs, EntryQPs)
% 	;   PSs == nonquery -> 
% 	    query_predicates_of_gprog(CQPs, UncondQPs, EntryQPs)
% 	;   make_entry_name(entry(_), Name, EName),
% 	    EntryQPs = [EName/Arity|EntryQPs0],
% 	    (   PSs = [] -> UncondQPs = [Func|UncondQPs0]
% 	    ;   UncondQPs = UncondQPs0
% 	    ),
% 	    query_predicates_of_gprog(CQPs, UncondQPs0, EntryQPs0)
% 	).

% collect_inner_preds(Program, DescGraph, InnerPreds) :-
% 	findall(Func, inner_pred(Program, DescGraph, Func), InnerPreds0),
% 	sort(InnerPreds0, InnerPreds).

% inner_pred(Program, DescGraph, Func) :-
% 	(   length(Program,_) -> true
% 	),			% to close the open-ended list
% 	Arity = 1,
% 	member(_F-Pred, Program),
% 	member(cl(_Head, Body, _AMap), Pred),
% 	member(goal(G,Alias), Body),
% 	(   Alias = [_|_] -> true
% 	;   Alias = []
% 	),
% 	functor(G, Name, Arity),
% 	Func0 = Name/Arity,
% 	(   Func = Func0
% 	;   member(Func0-LDF, DescGraph),
% 	    Func = _/1,
% 	    member(Func, LDF)
% 	).

%---------------------------------------------------------------------------
% Extending the program to include orphans and abox only preds and add
% pdata(...) terms needed for unfolding.

program_to_dprogram(Program0, AncDeps, ABoxSign0, DProgram) :-
	unzip1(Program0, PredFuncs0),
	sort(PredFuncs0, PredFuncs),
	sort(ABoxSign0, ABoxSign),
	ord_subtract(ABoxSign, PredFuncs, ABoxOnly),
	ord_subtract(PredFuncs, ABoxSign, PredsNotInABox),
	findall(Func, anr_predicate(AncDeps, PredFuncs, Func), ANRFuncs),
	program_to_dprogram1(Program0, AncDeps, ANRFuncs, PredsNotInABox, DProgram0, DProgram1),
	findall(Pred,
		predicate_with_no_tbox_clauses(PredFuncs, ABoxOnly, AncDeps, Pred),
		DProgram1),
	sort(DProgram0, DProgram).

anr_predicate(AncDeps, _PredFuncs, Func) :-
	LAdepFs = [_|_],
	member(Func-LAdepFs, AncDeps),
	negated_functor(Func, NFunc),
	memberchk(NFunc, LAdepFs).

program_to_dprogram1([], _AncDeps, _ANRFuncs, _PredsNotInABox) --> [].
program_to_dprogram1([Pred|Preds], AncDeps, ANRFuncs, PredsNotInABox) -->	
	pred_to_dpred(Pred, AncDeps, ANRFuncs, PredsNotInABox),
	program_to_dprogram1(Preds, AncDeps, ANRFuncs, PredsNotInABox).

pred_to_dpred(Func-Cls0, AncDeps, ANRFuncs, PredsNotInABox, [pr(Func,Cls0,PData)|DProgram0], DProgram0) :-
	length(Cls0, ClNum0),
	PData = pdata(ClNum,ABox,Rec,ADep),
	(   member(Func, PredsNotInABox) -> ClNum is ClNum0+1, ABox = no_abox_clauses
	;   ClNum is ClNum0+2, ABox = abox_clauses
	),
	(   member(Func, ANRFuncs) -> Rec = anr
	;   Rec = no_anr
	),
	(   memberchk(Func-ADep, AncDeps) -> true
	;   ADep = []
	).

predicate_with_no_tbox_clauses(PredFuncs, ABoxOnly, ADeps, pr(Func,[],PData)) :-
	PData = pdata(0,ABox,Rec,ADep),
	(   member(Func, ABoxOnly),
	    memberchk(Func-ADep, ADeps),
	    ABox = abox_clauses,
	    (   ADep == [] -> Rec = no_anr
	    ;   Rec = anr
	    )
	;   ADep = [_],
	    member(Func-ADep, ADeps),
	    \+ member(Func, ABoxOnly),
	    \+ member(Func, PredFuncs),
	    ABox = no_abox_clauses,
	    Rec = anr
	).

%---------------------------------------------------------------------------
% Ordering clause bodies

order_clause_bodies_program([], _Dict, []).
order_clause_bodies_program([pr(F,Cls0,PData)|DProgram0], Dict,
		    [pr(F,Cls,PData)|DProgram]) :-
	order_clause_bodies_predicate(Cls0, Dict, Cls),
	order_clause_bodies_program(DProgram0, Dict, DProgram).

order_clause_bodies_predicate([], _, []).
order_clause_bodies_predicate([Cl0|Cls0], Dict, [Cl|Cls]) :-
	order_clause_body(Cl0, Dict, Cl),
	order_clause_bodies_predicate(Cls0, Dict, Cls).

order_clause_body(cl(Head,Body0,AMap), Dict, cl(Head,Body,AMap)) :-
	order_goals(Body0, Dict, Body).

order_goals(Goals, Dictionary, OGoals) :-
	pair_with_sizes(Goals, Dictionary, PGoals),
	keysort(PGoals, SGoals),
	unzip2(SGoals, OGoals).

pair_with_sizes([], _, []).
pair_with_sizes([G|Gs], Dictionary, [S-G|SGs]) :-
	functor_of_goal(G, F),
	predicate_size(F, S, Dictionary),
	pair_with_sizes(Gs, Dictionary, SGs).

functor_of_goal(goal(G, _), Name/Arity) :-
	functor(G, Name, Arity).

predicate_size(F, S, Dictionary) :-
	dict_memberchk(pr(F,_,pdata(S,_,_,_)), Dictionary), !.
predicate_size(_, S, _) :-
	S = -2.

% ktf = known_to_fail

specialise_entry_preds(TPreds0, TPreds, PSs) :-
	initial_predicate_sensitivities(TPreds0, PSs0),
	remove_clauses_known_to_fail(TPreds0, TPreds, PSs0, PSs1),
	semi_orphan_preds(TPreds, SOFuncs),
	add_semi_orphan_sensitivities(TPreds, SOFuncs, PSs1, PSs).

semi_orphan_preds([], []).
semi_orphan_preds([pr(Func,[],pdata(_,abox_clauses,_,[_|_]))|TPreds], SOFuncs) :-
	!, SOFuncs = [Func|SOFuncs1],
	semi_orphan_preds(TPreds, SOFuncs1).
semi_orphan_preds([_|TPreds], SOFuncs) :-
	semi_orphan_preds(TPreds, SOFuncs).

add_semi_orphan_sensitivities([], _SOFuncs, PSs, PSs).
add_semi_orphan_sensitivities([pr(ParentFunc,Cls0,_PData)|Preds], Funcs, PSs0, PSs) :-
	findall(F, sensitivity_of_one_of_functors_called_in(Cls0, Funcs, PSs0, F), Fs0),
	sort(Fs0, Fs),
	add_sensitivity(Fs, ParentFunc, PSs0, PSs1),
	add_semi_orphan_sensitivities(Preds, Funcs, PSs1, PSs).

sensitivity_of_one_of_functors_called_in(Cls0, Funcs, PSs0, SF) :-
	goal_in_predicate_tree(Cls0, Goal),
	goal_to_func_ancs(Goal, Func, _),
	memberchk(Func, Funcs),
	memberchk(Func-Ss, PSs0),
	member(SF, Ss).

initial_predicate_sensitivities([], []).
initial_predicate_sensitivities([pr(Func,_,pdata(_,_,Rec,_))|TPreds], PSs) :-
	(   Rec == anr ->
	    negated_functor(Func, NFunc),
	    PSs = [Func-[NFunc]|PSs1]
	;   PSs1 = PSs
	),
	initial_predicate_sensitivities(TPreds, PSs1).

remove_clauses_known_to_fail(TPreds0, TPreds) -->
	{empty_preds_in_program(TPreds0, NonEmptyTPreds, TPreds, TPredsTail, Funcs)},
	{Funcs \== []}, !,
	remove_clauses_ktf_from_program(NonEmptyTPreds, Funcs, TPreds1),
	remove_clauses_known_to_fail(TPreds1, TPredsTail).
remove_clauses_known_to_fail(TPreds, TPreds) --> [].

empty_pred(pr(Func,[],pdata(_,no_abox_clauses,_,_)), Func).

empty_preds_in_program([Pred|Preds0], NonEmptyPreds, AllPreds, AllPredsTail, Funcs) :-
	(   empty_pred(Pred, Func) ->
	    AllPreds = [Pred|AllPreds0],
	    Funcs = [Func|Funcs0],
	    empty_preds_in_program(Preds0, NonEmptyPreds, AllPreds0, AllPredsTail, Funcs0)
	;   NonEmptyPreds = [Pred|NonEmptyPreds0],
	    empty_preds_in_program(Preds0, NonEmptyPreds0, AllPreds, AllPredsTail, Funcs)
	).
empty_preds_in_program([], [], AllPreds, AllPreds, []).

remove_clauses_ktf_from_program([], _Funcs, []) --> [].
remove_clauses_ktf_from_program([pr(ParentFunc,Cls0,PData)|Preds0], Funcs,
				[pr(ParentFunc,Cls,PData)|Preds]) -->
	add_predicate_ancdeps(ParentFunc, PData, ParentInfo),
	remove_clauses_ktf(Cls0, ParentInfo, Funcs, Cls),
	remove_clauses_ktf_from_program(Preds0, Funcs, Preds).

add_predicate_ancdeps(ParentFunc, PData, ParentInfo, PSs, PSs) :-
	(   PSs == eqtrans ->
	    PData = pdata(_,_,_,PredADep),
	    ParentInfo = eqtrans(PredADep)
	;   ParentInfo = ParentFunc
	).

remove_clauses_ktf(Cls0, ParentInfo, Funcs, Cls) -->
	{select(Cl, Cls0, Cls1)},
	clause_known_to_fail(ParentInfo, Cl, Funcs), !,
	remove_clauses_ktf(Cls1, ParentInfo, Funcs, Cls).
remove_clauses_ktf(Cls, _, _, Cls) --> [].

clause_known_to_fail(eqtrans(PredADep), Cl, Funcs, PSs0, PSs) :- !,
	goal_in_clause_tree(Cl, Goal),
	(   goal_to_func_ancs(Goal, Func, AncsL),
	    memberchk(Func, Funcs),
	    negated_functor(Func, NFunc),
	    func_guaranteed_not_to_appear_on_anclist(NFunc, AncsL, PredADep)
	->  true
	;   Goal = tgoal('$checkanc',AncsL),
	    member([A], AncsL),
	    functor(A, Name, Arity),
	    \+ member(Name/Arity, PredADep)
	),
	PSs = PSs0, !.
clause_known_to_fail(ParentFunc, Cl, Funcs, PSs0, PSs) :-
%	head_var_of_clause(Cl, X),
	goal_in_clause_tree(Cl, Goal),
	(   goal_to_func_ancs(Goal, Func, AncsL),
	    memberchk(Func, Funcs),
	    sensitivity(Func, PSs0, S),
	    no_ancs_contain_any_of(AncsL, S) -> true
	;   Goal = tgoal('$checkanc',AncsL),
	    member([A], AncsL),
%	    arg(1, A, Y), Y == X,
	    functor(A, Name, Arity),
	    S = [Name/Arity]
	), !,
	add_sensitivity(S, ParentFunc, PSs0, PSs).

% head_var_of_clause(tcl(_NLit,_N,Ts), X) :-
%	Ts = [t(_,_,X)|_].

sensitivity(Func, PSs, S) :-
	memberchk(Func-S, PSs), !.
sensitivity(_Func, _PSs, []).

add_sensitivity([], _Func, PSs0, PSs) :- !,
	PSs = PSs0.
add_sensitivity(S0, Func, PSs0, PSs) :-
	(   select(Func-OldS, PSs0, PSs1) -> true
	;   PSs1 = PSs0, OldS = []
	),
	ord_union(S0, OldS, NewS),
	PSs = [Func-NewS|PSs1].

tree_program_to_clause_program([], _Ctxt, _Env, Prog0, Prog0).
tree_program_to_clause_program([pr(Func,TCls,pdata(_,ABox,_,PredADep))|TProg], Ctxt, Env, Prog, Prog0) :-
	(   exclude_tree_pred(Func, Ctxt) -> Prog = Prog1
%	;   TCls == [], ABox == no_abox_clauses -> Prog = Prog1
	;   Func = _/2 -> Prog = Prog1
	;   tree_clauses_to_clauses(TCls, Func, PredADep, Ctxt, Env, Cls0),
	    make_entry_functor(Func, Ctxt, NewFunc),
	    (   Func == NewFunc -> Cls = Cls0
	    ;   ABox = no_abox_clauses -> Cls = Cls0
	    ;   top_predicate(TCls) -> Cls = Cls0
	    ;   abox_clause(NewFunc, Func, Cl),
		Cls = [Cl|Cls0]
	    ),
	    Prog = [NewFunc-Cls|Prog1]
	),
	tree_program_to_clause_program(TProg, Ctxt, Env, Prog1, Prog0).

top_predicate([tcl(_NLit,_N,[T])]):-
	empty_tree(T, _).

empty_tree(t([],[],X), X).

exclude_tree_pred(Func, ctxt(inner(InnerPreds),_,_)) :-
	\+ member(Func, InnerPreds).
exclude_tree_pred(Func, ctxt(entry(EntryPreds),_,_)) :-
	\+ member(Func, EntryPreds).

abox_clause(NewPredName/1, PredName/1, cl(Head,Body,[])) :-
	Head =.. [NewPredName,X],
	G =.. [PredName,X],
	Body = [goal(G, abox)].

tree_clauses_to_clauses([], _Func, _PredADep, _Ctxt, _Env, []).
tree_clauses_to_clauses([TCl|TCls], Func, PredADep, Ctxt, Env, [Cl|Cls]) :-
	tree_clause_to_clause(TCl, Func, PredADep, Ctxt, Env, Cl),
	tree_clauses_to_clauses(TCls, Func, PredADep, Ctxt, Env, Cls).

tree_clause_to_clause(TCl, PredFunc, PredADep, Ctxt0, Env, cl(Head,Body,AMap)) :-
	copy_term(TCl, tcl(_NLit,_N,Ts)),
	Ts = [t(_,_,X)|_],
	make_entry_head(PredFunc, X, Ctxt0, Head),
	update_context_recinfo(Ctxt0, PredFunc, PredADep, Head, Ctxt),
	trees_to_goals(Ts, Env, Ctxt, AMap0, Body, []),
	transform_amap(AMap0, AMap).

update_context_recinfo(ctxt(Mode,CQPs,RPs), Func, ParentADep,
		       Head, ctxt4(Mode,CQPs,RecInfo,ParentADep)) :-
	(   memberchk(Func-RecSuccs, RPs) ->
	    RecInfo = rec(Head,RecSuccs)
	;   RecInfo = nonrec
	).	

make_entry_head(PredName0/_, X, ctxt(Mode,_,_), Head) :-
	make_entry_name(Mode, PredName0, PredName),
	Head =.. [PredName,X].

make_entry_functor(PredName0/Arity, ctxt(Mode,_,_), PredName/Arity) :-
	make_entry_name(Mode, PredName0, PredName).

make_entry_name(single, PredName0, PredName) :-
	!, PredName = PredName0.
make_entry_name(entry(_), PredName0, PredName) :-
	!, PredName = PredName0.
make_entry_name(_, PredName0, PredName) :-
	atom_concat(PredName0, '$', PredName).

make_goal_name(single, _, Name0, Name) :- !,
	Name = Name0.
make_goal_name(_, abox, Name0, Name) :- !,
	Name = Name0.
make_goal_name(_, entry, Name0, Name) :-
	!, Name = Name0.
make_goal_name(_, _, Name0, Name) :-
	atom_concat(Name0, '$', Name).

internalise_anc(single, Anc, IAnc) :- !,
	IAnc = Anc.
internalise_anc(_Mode, Anc, IAnc) :-
	functor(Anc, Name, 1),
	make_entry_name(internal, Name, IName),
	functor(IAnc, IName, 1),
	arg(1, Anc, X),
	arg(1, IAnc, X).

internalise_ancs([], _Mode, []).
internalise_ancs([A|As], Mode, [IA|IAs]) :-
	internalise_anc(Mode, A, IA),
	internalise_ancs(As, Mode, IAs).

opt_deinternalise_name(IName, Name) :-
	atom_concat(Name, '$', IName), !.
opt_deinternalise_name(Name, Name).


trees_to_goals([], _, _, []) --> [].
trees_to_goals([T|Ts], Env, Ctxt, AMap) -->
	tree_to_goals(T, Env, Ctxt, AMap0, AMap),
	trees_to_goals(Ts, Env, Ctxt, AMap0).

tree_to_goals(t(Conds,FuncTrees,V), Env, Ctxt, AMap0, AMap) -->
	conds_to_goals(Conds, V, Env, Ctxt, AMap0, AMap1),
	func_trees_to_goals(FuncTrees, V, Env, Ctxt, AMap1, AMap).

func_trees_to_goals([FT|FTs], V, Env, Ctxt, AMap0, AMap) -->
	func_tree_to_goals(FT, V, Env, Ctxt, AMap0, AMap1),
	func_trees_to_goals(FTs, V, Env, Ctxt, AMap1, AMap).
func_trees_to_goals([], _, _Env, _Ctxt, AMap0, AMap0) --> [].

func_tree_to_goals(Func/Inv-T, V, Env, Ctxt, AMap0, AMap) -->
	{T = t(_,_, W),
	 (   Inv = role -> Args = [V,W]
	 ;   Args = [W,V]
	 ),
	 G =.. [Func|Args],
	 Goal = goal(G, [])},
	[Goal],
	tree_to_goals(T, Env, Ctxt, AMap0, AMap).

conds_to_goals([], _, _, _, AMap, AMap) --> [].
conds_to_goals([Cond|Conds], V, Env, Ctxt, AMap0, AMap) -->
	cond_to_goal(Cond, V, Env, Ctxt, AMap0, AMap1),
	conds_to_goals(Conds, V, Env, Ctxt, AMap1, AMap).

cond_to_goal(TGoal, V, Env, Ctxt, AMap0, AMap) -->
	{TGoal = tgoal(Name,AncsL)},
	(   {Name = vareq(_)} ->
	    {AMap = AMap0}
	;   {Name == '$checkanc'} ->
	    {AMap = AMap0},
	    {Ctxt = ctxt4(Mode,_,_,_)},
	    check_anc_goals(AncsL, Mode)
	;   {cond_to_goal1(Name, AncsL, V, Ctxt, Goal, Env, AMap0, AMap)},
	    (   {Goal \== none} ->
		[Goal]
	    ;   []
	    )
	).

check_anc_goals([[A]|AncsL], Mode) -->
	{internalise_anc(Mode, A, IA)},
	['$checkanc'(IA)],
	check_anc_goals(AncsL, Mode).
check_anc_goals([], _) --> [].

cond_to_goal1(Name, AncsL, V, ctxt4(Mode,CQPs,RecInfo,ParentADep), Goal, Dict, AMap0, AMap) :-
	(   AncsL = [abox] -> AMap = AMap0, G =.. [Name,V], Goal= goal(G, abox)
	;   Func = Name/1,
	    dict_memberchk(pr(Func,Cls,PData), Dict), 
	    (   orphan(Cls, PData) ->
		negated_predname(Name, NName),
		A =.. [NName,V], AMap = AMap0,
		internalise_anc(Mode, A, IA),
		(   AncsL == [[]]  -> Goal = '$checkanc'(IA),
		    (   Mode = entry(_) -> 
			my_error('Intrnl. Error in cond_to_goal1, $checkanc in entry clause: Name=~q, AncsL=~q',
				 [Name,AncsL])
		    ;   true
		    )
		;   AncsL = [[B]] ->
		    (   Mode = entry(_) -> A = B, Goal = none%, my_error(xxxx, [])
		    ;   internalise_anc(Mode, B, IB),
			Goal = '$checkanc'(IA, IB)
		    )
		;   my_error('Intrnl error in cond_to_goal1, Name=~q, AncsL=~q',
			   [Name,AncsL]), fail
		)
	    ;   (   memberchk(Name/1-query(SFs), CQPs) ->
		    (   Mode = entry(_), no_ancs_contain_any_of(AncsL, SFs) ->
			entry_ancslist(Cls, Func, PData, AncsArg0), AMap = AMap0
		    ;   member(SF, SFs),
			\+ func_guaranteed_not_to_appear_on_anclist(SF, AncsL, ParentADep) ->
			alias_ancslist(AncsL, PData, Mode, AncsArg0, AMap0, AMap)
		    ;   entry_ancslist(Cls, Func, PData, AncsArg0), AMap = AMap0
		    )
		;   alias_ancslist(AncsL, PData, Mode, AncsArg0, AMap0, AMap)
		),
		(   RecInfo = rec(ParentHead,RecSuccs),
		    memberchk(Name/1, RecSuccs) ->
		    AncsArg = rec(ParentHead,AncsArg0)
		;   AncsArg = AncsArg0
		),
		make_goal_name(Mode, AncsArg0, Name, GName),
		G =.. [GName,V],
		Goal = goal(G, AncsArg)
	    )
	).

orphan([], pdata(_,no_abox_clauses,anr,_)).

no_ancs_contain_any_of(AncsL, SF) :-
	(   member(Ancs, AncsL),
	    member(A, Ancs),
	    functor(A, Name, Arity),
	    memberchk(Name/Arity, SF) -> fail
	;   true
	).

% This is called for a predicate p only when not_p cannot appear
% on the Anc list. Therefore it is safe to generate an abox call
% even if p is ANR.
entry_ancslist([], Func, PData, AncsArg) :- !,
	(   PData = pdata(_,abox_clauses,_/*no_anr*/,_) ->
	    AncsArg = abox
   	;   my_error('Intrnl error: entry_ancslist([], ~w,...): empty pred',
		   [Func]), AncsArg = entry
	).
entry_ancslist(_Cls, _Func, _PData, entry).

func_guaranteed_not_to_appear_on_anclist(Func, AncsL, ParentADep) :-
	(   memberchk(Func, ParentADep) 
	    % the parent predicate can be reached from Func
	->  fail
	;   member(Ancs, AncsL),
	    Func = Name/Arity,
	    functor(Anc, Name, Arity),
	    member(Anc, Ancs)
	    % Func does appear on the local AncsL
	->  fail
	;   true
	).


alias_ancslist([Ancs0|AncsL], PData, Mode, [AAncs|AAncsL], AMap0, AMap) :-
	cond_ancestors1(Ancs0, PData, Ancs),
%	(   Ancs == Ancs0 -> true
%	;   write(Ancs0-Ancs), nl
%	),
	internalise_ancs(Ancs, Mode, IAncs),
	alias_ancs(IAncs, AAncs, AMap0, AMap1),
	alias_ancslist(AncsL, PData, Mode, AAncsL, AMap1, AMap).
alias_ancslist([], _, _, [], AMap, AMap).

alias_ancs(Ancs0, Alias, AMap0, [Ancs=Alias|AMap0]) :-
	sort(Ancs0, Ancs).

filter_query_preds_entry_only_mode([], _Dict, []).
filter_query_preds_entry_only_mode([Func-Conds|CQPs0], Dict, CQPs) :-
	(   dict_memberchk(pr(Func,_,pdata(_,_,anr,_)), Dict) ->
	    filter_query_preds_entry_only_mode(CQPs0, Dict, CQPs)
	;   CQPs = [Func-Conds|CQPs1],
	    filter_query_preds_entry_only_mode(CQPs0, Dict, CQPs1)
	).

initial_query_pred_properties(RPreds, CQPs) :-
	findall(Func-nonquery, member(Func-_, RPreds), CQPs).

handle_query_preds(Preds0, CQPs0, PSs, Ctxt, CQPs) :-
	select(Pred, Preds0, Preds1),
	decide_query_pred(Pred, PSs, CQPs0, Ctxt, CQPs1), !,
	handle_query_preds(Preds1, CQPs1, PSs, Ctxt, CQPs).
handle_query_preds(_Preds, CQPs, _PSs, _Ctxt, CQPs).

decide_query_pred(pr(F,Cls,_), PSs, CQPs0, Ctxt, CQPs) :-
	query_predicate_body(Cls, Ctxt, CQPs0, Result0, []),
	(   memberchk(nonquery, Result0) -> Result = nonquery
	;   sensitivity(F, PSs, Sensty0),
	    ord_union([Sensty0|Result0], Sensty),
	    Result = query(Sensty)
	),
	CQPs =[F-Result|CQPs0].

query_predicate_body([tcl(_,_,[T])|Cls], Ctxt, CQPs) -->
	query_predicate_clause_tree(T, Ctxt, CQPs),
	query_predicate_body(Cls, Ctxt, CQPs).
query_predicate_body([], _Ctxt, _CQPs) --> [].

query_predicate_clause_functrees([], _Ctxt, _CQPs) --> [].
query_predicate_clause_functrees([_Func-T|FTs], Ctxt, CQPs) -->
	query_predicate_clause_tree(T, Ctxt, CQPs),
	query_predicate_clause_functrees(FTs, Ctxt, CQPs).

query_predicate_clause_tree(t(Cs,FTs,_), Ctxt, CQPs) -->
	query_predicate_conds(Cs, Ctxt, CQPs),
	query_predicate_clause_functrees(FTs, Ctxt, CQPs).

query_predicate_conds([], _Ctxt, _CQPs) --> [].
query_predicate_conds([C|Cs], Ctxt, CQPs) -->
	query_predicate_cond(C, Ctxt, CQPs),
	query_predicate_conds(Cs, Ctxt, CQPs).

query_predicate_cond(Goal, Ctxt, CQPs) -->
	{goal_to_name_func_ancs(Goal, Name, Func, AncsL)},
	(   {AncsL == [abox]} -> []
	;   {spec_goal(Name)} -> []
	;   {query_pred_property(Func, CQPs, Ctxt, Prop)},
	    (   {Sensty == nonquery} -> [nonquery]
	    ;   {Prop = query(Sensty)},
	        (   {member(Ancs, AncsL),
		     retain_only_those_with_functor_in(Ancs, Sensty, SAncs),
		     SAncs \== []
		    } -> [nonquery]
		;   [Sensty]
		)
	    )
	).

spec_goal(vareq(_)).

query_pred_property(Func, PredProps, _Ctxt, Prop) :-
	memberchk(Func-Prop, PredProps).


unfold_preds(Preds, Env, UPreds) :-
	unfold_preds0(Preds, Env, UPreds0),
	recalculate_ancdeps(UPreds0, UPreds1),
	sort(UPreds1, UPreds).
%	UPreds = UPreds0.


recalculate_ancdeps(UPreds0, UPreds) :-
	renew_ancdeps(UPreds0, UPreds1, ChangedADeps),
	(   ChangedADeps \== yes ->
	    UPreds = UPreds0
	;   remove_clauses_known_to_fail(UPreds1, UPreds2, eqtrans, _),
%  	    list_to_dict(UPreds2, Dict),
%  	    renew_ancsargs_prog(UPreds2, Dict, UPreds3),
%	    recalculate_ancdeps(UPreds3, UPreds)
	    recalculate_ancdeps(UPreds2, UPreds)
	).

renew_ancsargs_prog([pr(Func,Cls0,PData)|Preds0], Dict, [pr(Func,Cls,PData)|Preds]) :-
	PData = pdata(_,_,_,ParentADep),
	renew_ancsargs_clauses(Cls0, ParentADep, Dict, Cls),
	renew_ancsargs_prog(Preds0, Dict, Preds).
renew_ancsargs_prog([], _, []).

renew_ancsargs_clauses([], _ParentADep, _Dict, []).
renew_ancsargs_clauses([Cl0|Cls0], ParentADep, Dict, Cls) :-
	(   renew_ancsargs_clause(Cl0, ParentADep, Dict, Cl) ->
	    Cls = [Cl|Cls1],
	    renew_ancsargs_clauses(Cls0, ParentADep, Dict, Cls1)
	;   renew_ancsargs_clauses(Cls0, ParentADep, Dict, Cls)
	).

renew_ancsargs_clause(tcl(NLit,N,Ts0), ParentADep, Dict, tcl(NLit,N,Ts)) :-
	renew_ancsargs_trees(Ts0, ParentADep, Dict, Ts).

renew_ancsargs_trees([T0|Ts0], ParentADep, Dict, [T|Ts]) :-
	renew_ancsargs_tree(T0, ParentADep, Dict, T),
	renew_ancsargs_trees(Ts0, ParentADep, Dict, Ts).
renew_ancsargs_trees([], _, _, []).

renew_ancsargs_tree(t(Conds0,FuncTrees0,V), ParentADep, Dict,
		    t(Conds,FuncTrees,V)) :-
	renew_ancsargs_conds(Conds0, ParentADep, Dict, Conds),
	renew_ancsargs_functrees(FuncTrees0, ParentADep, Dict, FuncTrees).

renew_ancsargs_functrees([Func-Tree0|FuncTrees0], ParentADep, Dict,
			 [Func-Tree|FuncTrees]) :-
	renew_ancsargs_tree(Tree0, ParentADep, Dict, Tree),
	renew_ancsargs_functrees(FuncTrees0, ParentADep, Dict, FuncTrees).
renew_ancsargs_functrees([], _, _, []).

renew_ancsargs_conds([Cond0|Conds0], ParentADep, Dict, [Cond|Conds]) :-
	renew_ancsargs_cond(Cond0, ParentADep, Dict, Cond),
	renew_ancsargs_conds(Conds0, ParentADep, Dict, Conds).
renew_ancsargs_conds([], _, _, []).

renew_ancsargs_cond(tgoal(Name,AncsL0), ParentADep, Dict, tgoal(Name,AncsL)) :-
	(   spec_goal(Name) -> AncsL = AncsL0
	;   Name == '$checkanc' ->
	    (   member([Anc], AncsL0), functor(Anc, Name, Arity),
		\+ member(Name/Arity, ParentADep) -> fail
	    ;   AncsL = AncsL0
	    )
	;   dict_memberchk(pr(Name/1,_,pdata(_,_,_,ADep)), Dict), 
	    renew_ancsargs(AncsL0, ADep, Dict, AncsL1),
	    sort(AncsL1, AncsL)
	).

renew_ancsargs([], _, _, []).
renew_ancsargs([Ancs0|AncsL0], ADep, Dict, [Ancs|AncsL]) :-
	retain_only_those_with_functor_in(Ancs0, ADep, Ancs),
	renew_ancsargs(AncsL0, ADep, Dict, AncsL).

renew_ancdeps(Preds0, Preds, ChangedADeps) :-
	sort(Preds0, Preds1),
	ancres_dependency_graph_of_tree_program(Preds1, AncDeps),
	renew_ancdeps1(Preds1, AncDeps, Preds, ChangedADeps).

renew_ancdeps1([pr(Func,Cls,PData0)|Preds0], AncDeps,
	       [pr(Func,Cls,PData)|Preds], ChangedADeps) :-
	(   select(Func-ADep, AncDeps, AncDeps1) -> true
	;   ADep = [], AncDeps1 = AncDeps
	),
	PData0 = pdata(ClNum,ABox,Rec0,ADep0),
	PData =  pdata(ClNum,ABox,Rec,ADep),
	(   ADep == ADep0 -> Rec = Rec0
	;   ChangedADeps = yes,
	    negated_functor(Func, NFunc),
	    (   memberchk(NFunc, ADep) -> Rec = anr
	    ;   Rec = no_anr
	    )
	),
	renew_ancdeps1(Preds0, AncDeps1, Preds, ChangedADeps).
renew_ancdeps1([], _, [], _).

unfold_preds0([Pred|Preds], Env, [UPred|UPreds]) :-
	unfold_pred(Pred, Env, UPred),
	unfold_preds0(Preds, Env, UPreds).
unfold_preds0([], _, []).

unfold_pred(pr(Func,_,PData), Env, pr(Func,UCls,PData)) :-
	init_clause_count(Env),
	PData = pdata(_,_,_,ADep),
	findall(UCl, unfolded_clause_of_pred(Func, ADep, Env, UCl), UCls0),
	remove_redundant_clauses(UCls0, UCls).

unfolded_clause_of_pred(Func, ADep, Env, ClTree) :-
	arg(3, Env, ADep),
	make_goal(Func, G, X),
	unfold_top_goal(G, X, Env, Goals0),
	unfold_further_primary_goals(Goals0, X, Env, Goals),
	goals_to_clause_tree(Goals, G, Env, ClTree),
	clause_count.

init_clause_count(env(_,Opts,_)) :-
	opt_member(Opts, 30, max_clauses_generated(MaxCl)),
	bb_put(cl_count, MaxCl).

clause_count :-
	bb_get(cl_count, N),
	N1 is N-1,
	bb_put(cl_count, N1).

check_clause_count :-
	bb_get(cl_count, N), N>0.

unfold_further_primary_goals(Goals0, HeadVar, Env, Goals) :-
	Env = env(_,Opts,_),
	opt_member(Opts, max, primary(Primary)),
	opt_member(Opts, 2, max_clauses_unfolded(MaxClauses)),
	unfold_further_primary_goals(Goals0, HeadVar, Primary, MaxClauses, Env, Goals).

unfold_further_primary_goals(Goals0, HeadVar, Primary, MaxClauses, Env, Goals) :-
	(   Primary == mid -> \+ contains_a_binary_goal_with_var(HeadVar, Goals0)
	;   Primary == max
	),
	check_clause_count,
	unary_goals_with_var(Goals0, HeadVar, Conds, Goals2),
	select(Goal, Conds, Goals1),
	Goal = goal(G,A), A \== abox,
	clause_limits(G, MaxClauses, Env), !,
	append(Goals1, Goals2, Goals12),
	unfold_goals([Goal|Goals12], HeadVar, Env, Goals3),
	unfold_further_primary_goals(Goals3, HeadVar, Primary, MaxClauses, Env, Goals).
unfold_further_primary_goals(Goals, _, _, _, _, Goals).

clause_limits(G, MaxClauses, env(Dictionary,_,_)) :-
	functor(G, Name, Arity),
	dict_memberchk(pr(Name/Arity,_,pdata(ClNum,_,_,_)), Dictionary),
	ClNum =< MaxClauses+1.

contains_a_binary_goal_with_var(X, Goals) :-
	member(goal(G,_), Goals),
	functor(G, _, 2),
	(   arg(1, G, X) -> true
	;   arg(2, G, X)
	), !.

unary_goals_with_var(Gs0, X, Conds, Gs) :-
	select(goal(G,Ancs), Gs0, Gs1),
	G =.. [_,U], U == X, !,
	Conds = [goal(G,Ancs)|Conds0],
	unary_goals_with_var(Gs1, X, Conds0, Gs).
unary_goals_with_var(Gs, _, [], Gs).

make_goal(Name/Arity, Head, HeadVar) :-
	functor(Head, Name, Arity),
	arg(1, Head, HeadVar).

unfold_top_goal(G, X, Env, UGoals) :-
	Env = env(_,Opts,_),
	(   memberchk(primary(none), Opts) ->
	    do_not_unfold_top_goal(G, Env, UGoals)
	;   clause_matching(goal(G,[]), Env, [], Goals1, End),
	    (   End == end -> UGoals = Goals1
	    ;   unfold_goals(Goals1, X, Env, UGoals)
	    )
	).

do_not_unfold_top_goal(G, Env, UGoals) :-
	clause_matching1(G, [], Env, Body, AMap),
	process_amap(AMap, []),
	add_goals_no_unfold(Body, [], UGoals).

add_goals_no_unfold([]) --> [].
add_goals_no_unfold([goal(G,AncsL)|Goals]) -->
	add_goal1(AncsL, G),
	add_goals_no_unfold(Goals).

unfold_goals([Goal|Goals0], X, Env, UGoals) :-
	(   non_unfoldable_goal(Goal, X) ->
	    UGoals = [Goal|UGoals1],
	    unfold_goals(Goals0, X, Env, UGoals1)
	;   clause_matching(Goal, Env, Goals0, Goals1, End),
	    (   End == end -> UGoals = Goals1
	    ;   unfold_goals(Goals1, X, Env, UGoals)
	    )
	).
unfold_goals([], _, _, []).

clause_matching(goal(G,As), Env, Goals0, Goals1, End) :-
	clause_matching1(G, As, Env, Body, AMap),
	process_amap(AMap, As),
	add_body(Body, Env, Goals0, Goals1, End).

add_body([], _Env, Goals, Goals, _).
add_body([Goal|Body0], Env, Goals0, Goals, End) :-
	add_body(Body0, Env, Goals0, Goals1, End),
	add_goal(Goal, Env, Goals1, Goals, End).

add_goal(G, _Env, Goals0, Goals, End) :-
	binary_goal(G, NG), !,
	Goals = [NG|Goals0], End = end.
add_goal(G, _Env, Goals0, Goals, End) :-
	G = goal(Goal,_),
	functor(Goal, '$checkanc', 1), !,
	Goals = [G|Goals0], End = end.
add_goal(G, _Env, Goals0, Goals, _End) :-
	check_anc(G), !, Goals = Goals0.
add_goal(G, _, _, _, _) :-
	check_loop(G), !, fail.
add_goal(goal(G, AncsL0), Env, Goals0, Goals, End) :-
	Env = env(Dictionary,_Opts,PredADep),
	(   AncsL0 == abox ->
	    % goal created during unfolding, see clause_matching1 (*)
	    add_goal2(G, abox, Goals0, Goals), End = end
	;   functor(G, Name, Arity), Func = Name/Arity,
	    dict_memberchk(pr(Func,Cls,pdata(_,ABox,_,ADep)), Dictionary) ->
	    negated_functor(Func, NFunc),
	    (   Cls = [_,_|_]
	    ->  add_goal1(AncsL0, G, Goals0, Goals)
	        % From now on we know that we have at most one clause
	    ;   func_guaranteed_not_to_appear_on_anclist(NFunc, AncsL0, PredADep) ->
		% From now on we know that ancres is not applicable		
		(   Cls == [] 
		->  (   ABox = no_abox_clauses -> !, fail % no TBox, no ABox, no anr
		    ;   ABox = abox_clauses -> % no TBox, no anr
			add_goal2(G, abox, Goals0, Goals), End = end
		    ;   add_goal1(AncsL0, G, Goals0, Goals)
		    )
		;   % we have a single clause 
		    ABox == no_abox_clauses,  % no ABox
		    AncsL0 = [Ancs],          % single Ancs
		    functor(GPat, Name, Arity),
		    \+ member(GPat, Ancs)     % non-recursive
		->  Cls = [Cl],
		    add_det_body(Cl, G, Ancs, ADep, Env, Goals0, Goals, End)
		;   add_goal1(AncsL0, G, Goals0, Goals)
		)
	    ;   Cls == [], ABox == no_abox_clauses, % orphan
		\+ memberchk(NFunc, PredADep) % from whose negation  the parent cannot be reached
	    ->	negated_goal_pattern(G, NG, _),
		perform_ancsres(AncsL0, NG, AncsL),
		(   AncsL == [] -> Goals = Goals0
		;   add_goal1(AncsL, G, Goals0, Goals)
		)
	    ;   add_goal1(AncsL0, G, Goals0, Goals)
	    )
	;   add_goal1(AncsL0, G, Goals0, Goals)
	).

add_det_body(Cl, G, Ancs, ADep, Env, Goals0, Goals, End) :-
	copy_term(Cl, cl(G,Body,AMap)),
	process_amap(AMap, Ancs),
	Env = env(Dict,Options,_),
	add_body(Body, env(Dict,Options,ADep), Goals0, Goals, End).

% assert_implies(Goal1, Goal2) :-
% 	Goal1, \+ Goal2, !,
% 	format('! assert_implies(~w,~w). does not hold~n', [Goal1,Goal2]).
% assert_implies(_,_).

perform_ancsres([], _G, []).
perform_ancsres([Ancs|AncsL0], G, AncsL) :-
	member(G, Ancs), !,
	perform_ancsres(AncsL0, G, AncsL).
perform_ancsres([Ancs|AncsL0], G, [Ancs|AncsL]) :-
	perform_ancsres(AncsL0, G, AncsL).

non_unfoldable_goal(goal(G,Ancs), X) :-
	(   Ancs == abox -> true
	;   functor(G, '$checkanc', 1) -> true
	;   functor(G, _, 2) -> true
	;   arg(1, G, Y), X \== Y
	).

retain_only_those_with_functor_in(abox, _, abox).
retain_only_those_with_functor_in([], _, []).
retain_only_those_with_functor_in([Anc0|Ancs0], LAdepFs, Ancs) :-
	functor(Anc0, Name, Arity),
	(   memberchk(Name/Arity, LAdepFs) ->
	    Ancs = [Anc0|Ancs1]
	;   Ancs = Ancs1
	),
	retain_only_those_with_functor_in(Ancs0, LAdepFs, Ancs1).

% classify_unary_goal(abox, _, end, abox) :-
% 	!.	 % goal created during unfolding, see clause_matching1 (*):
% classify_unary_goal([], pdata(ClNum,_,_,_), End, Ancs) :-
% 	(   ClNum < 0 -> !, fail
% 	;   ClNum =:= 0 -> !, Ancs = abox, End = end
% 	).
% classify_unary_goal(Ancs, _PData, _End, Ancs).

add_goal1([Ancs|AncsL], G, Goals0, Goals) :-
	(   Ancs == [] -> true
	;   Ancs = [_|_]
	), !,
	add_goal2(G, Ancs, Goals0, Goals1),
	(   AncsL == [] -> Goals = Goals1
	;   add_goal1(AncsL, G, Goals1, Goals)
	).
add_goal1(abox, G, Goals0, Goals) :- yyy,
	add_goal2(G, abox, Goals0, Goals).
add_goal1(AncsL, _G, _Goals0, _Goals) :- xxx(AncsL).

add_goal2(G0, A0, Goals0, Goals) :-
	select(Goal, Goals0, Goals1),
	Goal = goal(G1,A1),
	G1 == G0,
	collapsible_ancs(A0, A1, A), !,
	Goals = [goal(G0,A)|Goals1].
add_goal2(G, Ancs, Goals0, Goals) :-
	Goals = [goal(G,Ancs)|Goals0].

% check_anc(goal(G, AncsL)) :-
% 	AncsL = [_|_],
% 	negated_goal_pattern(G, NG, _),
% 	\+ (   member(Ancs, AncsL),
% 	       \+ (member(Anc, Ancs), Anc == NG)
% 	   ).

check_anc(goal(G, AncsL)) :-
	negated_goal_pattern(G, NG, _),
	check_anc(AncsL, NG).

check_anc([], _).
check_anc([Ancs|AncsL], NG) :-
	member(A, Ancs), A == NG,
	check_anc(AncsL, NG).
	

check_loop(goal(G, AncsL)) :-
	member(Ancs, AncsL),
	member(Anc, Ancs), G == Anc, !.

negated_functor(Name/Arity, NName/Arity) :-
	negated_predname(Name, NName).

negated_goal_pattern(G, NG, NP) :-
	G =.. [P|Args],
	negated_predname(P, NP),
	NG =.. [NP|Args].

negated_predname(P, NP) :-
	atom_concat('$not_', X, P), !, NP = X.
negated_predname(P, NP) :-
	atom_concat('$not_', P, NP).

binary_goal(goal(G,_), goal(G, [])) :-
	functor(G, _, 2).

clause_matching1(G, Top, env(Dictionary,_,PredADep), Body, AMap) :-
	functor(G, Name, Arity), Func = Name/Arity,
	dict_member(pr(Func,Clauses,PData), Dictionary),
	PData = pdata(_,ABox,Rec,_),
	(   Top \== [],            % do not add abox and ancres clause
	                           % to the unfolded pred itself
	    AMap = [],
	    (   Rec = anr,
		negated_goal_pattern(G, NG, NP),
		memberchk(NP/Arity, PredADep),
		arg(1, G, V),
		functor(CA, '$checkanc', 1),
		arg(1, CA, V),
		Body = [goal(CA,[NG])]
	    ;   ABox = abox_clauses,
		Body = [goal(G,abox)]   % (*)
	    )
	;   member(Cl, Clauses),
	    copy_term(Cl, cl(G,Body,AMap))
	).

process_amap([], _).
process_amap([AGoal|AGoals], AL) :-
	process_amap1(AGoal, AL),
	process_amap(AGoals, AL).
			      
process_amap1(na(AL0,AL1,AL2), _AL) :- 
	append(AL1, AL0, AL2).
%	\+ incorrect_ancs(_AL, AL2).
process_amap1(na(AL1,AL2), AL) :- 
	append(AL1, AL, AL2).
%	\+ incorrect_ancs(AL, AL2).

incorrect_ancs(AL0, AL) :-
	nonvar(AL0),
	member(A, AL), member(B, AL),
	arg(1, A, A1), arg(1, B, B1),
	A1 \== B1,
	inc_ancs(AL).

% ---------------------------------------------------------------------------
transform_amap(AMap0, AMap) :-
	sort(AMap0, AMap1),
	unify_aliases(AMap1, AMap2),
	transform_amap1(AMap2, init, AMap, []).

unify_aliases([], []).
unify_aliases([Ancs1=Alias1|AMap0], AMap) :-
	(   AMap0 = [Ancs2=Alias2|_], Ancs2 == Ancs1 ->
	    Alias1 = Alias2,
	    unify_aliases(AMap0, AMap)
	;   AMap = [Ancs1=Alias1|AMap1],
	    unify_aliases(AMap0, AMap1)
	).

transform_amap1([], _Start) --> !.
transform_amap1([[]=Alias|AMap0], Start) -->
	!, add_empty_alias(Start, Alias),
	transform_amap1(AMap0, Start).
transform_amap1(AMap0, Start) -->
	{ count_occurrences(AMap0, [], Occs),
	  max_occurrences(Occs, 0, 0, Elem),
	  partition_amap(AMap0, Elem, AMap1, AncsL, AMap2),
	  ord_intersection(AncsL, ISec),
	  (   Start == init -> NewAnc = na(ISec, NewStart)
	  ;   NewAnc = na(Start, ISec, NewStart)
	  ),
	  amap_remove_elems(AMap1, ISec, AMap1a)	
	},
	[NewAnc],
	transform_amap1(AMap1a, NewStart),
	transform_amap1(AMap2, Start).

add_empty_alias(Start, Alias) -->
	{ Start == init}, !, [na([], Alias)].
add_empty_alias(Start, Start) --> [].

count_occurrences([], St0, St) :- !, St = St0.
count_occurrences([L=_|LL], St0, St) :-
	count_occurrences1(L, St0, St1),
	count_occurrences(LL, St1, St).

count_occurrences1([], St0, St) :- !, St = St0.
count_occurrences1([E|L], St0, St) :-
	count_occurrence(St0, E, St1),
	count_occurrences1(L, St1, St).

count_occurrence([], E, [E-1]).
count_occurrence([E0-N|St0], E, St) :-
	(   E == E0 ->
	    N1 is N+1, St = [E0-N1|St0]
	;   St = [E0-N|St1],
	    count_occurrence(St0, E, St1)
	).

max_occurrences([], _, MaxElem, Elem) :-
	Elem = MaxElem.
max_occurrences([E-N|St0], Max, MaxElem, Elem) :-
	(   N > Max ->
	    max_occurrences(St0, N, E, Elem)
	;   max_occurrences(St0, Max, MaxElem, Elem)
	).

partition_amap([], _Elem, [], [], []).
partition_amap([Ancs=Al|AMap0], Elem, AMapWith, AncsL, AMapWO) :-
	(   ord_member(Elem, Ancs) ->
	    AMapWith = [Ancs=Al|AMapWith1],
	    AncsL = [Ancs|AncsL1],
	    partition_amap(AMap0, Elem, AMapWith1, AncsL1, AMapWO)
	;   AMapWO = [Ancs=Al|AMapWO1],
	    partition_amap(AMap0, Elem, AMapWith, AncsL, AMapWO1)
	).	    

amap_remove_elems([], _, []).
amap_remove_elems([Ancs0=Al|AMap0], TBRemoved, [Ancs=Al|AMap]) :-
	ord_subtract(Ancs0, TBRemoved, Ancs),
	amap_remove_elems(AMap0, TBRemoved, AMap).

% ---------------------------------------------------------------------------

remove_redundant_clauses([], UCls) :- !,
	UCls = [].
remove_redundant_clauses(UCls0, UCls) :-
	max_variables(UCls0, 0, MaxVno),
	length(Vars, MaxVno),
	share_variables(UCls0, Vars),
	sort(UCls0, UCls2),
	remove_redundant_clauses1(UCls2, [], UCls).

max_variables([Cl|Cls], Max0, Max) :-
	Cl = tcl(_,Max1,_),
	Max2 is max(Max0,Max1),
	max_variables(Cls, Max2, Max).
max_variables([], Max, Max).


remove_redundant_clauses1([], Cls, Cls).
remove_redundant_clauses1([Cl|Cls0], Cls1, Cls) :-
	is_a_superset_of_a_clause_in(Cl, Cls0), !,
	remove_redundant_clauses1(Cls0, Cls1, Cls).
remove_redundant_clauses1([Cl|Cls0], Cls1, Cls) :-
	is_a_superset_of_a_clause_in(Cl, Cls1), !,
	remove_redundant_clauses1(Cls0, Cls1, Cls).
remove_redundant_clauses1([Cl|Cls0], Cls1, Cls) :-
	remove_redundant_clauses1(Cls0, [Cl|Cls1], Cls).

is_a_superset_of_a_clause_in(Cl0, [Cl|Cls]) :-
	(   is_a_longer_clause(Cl, Cl0) -> fail
	;   is_a_subset_of(Cl, Cl0) -> true
	;   is_a_superset_of_a_clause_in(Cl0, Cls)
	).

is_a_longer_clause(tcl(Len1,_,_), tcl(Len2,_,_)) :-
	Len1 > Len2.

is_a_subset_of(tcl(_,Depth1,[T1]), tcl(_,Depth2,[T2])) :-
	Depth1 =< Depth2,
	ord_subtree(T1, T2).

share_variables([], _).
share_variables([X|Xs], Vars) :-
	term_variables_prefix(X, Vars),
	share_variables(Xs, Vars).

goals_to_clause_tree(Goals, Head, Env, tcl(NLit,N,Ts)) :-
	arg(1, Head, X),
	goals_to_trees1([X], Goals, 1-0, Env, N-NLit, Ts, Gs),
	(   Gs = [] -> true
	;   my_error('Internal error: goals_to_trees1(~w,~w,...) returns ~w',
		   [Goals,Head,Gs])
	).

goals_to_trees1([X|Xs], Gs0, N0, Env, N, [T|Ts], Gs) :-
	goals_to_tree(Gs0, [X], N0, Env, N1, T, Gs1),
	goals_to_trees1(Xs, Gs1, N1, Env, N, Ts, Gs).
goals_to_trees1([], Gs0, N0, _Env, N0, [], Gs0).

count_literals(Conds, FTrees, N0-L0, N-L) :-
	length(Conds, L1),
	length(FTrees, L2),
	L is L0+L1+L2,
	N is N0+L2.

goals_to_tree(Gs0, Xs, N0, Env, N,  T, Gs) :-
	goals_func_trees(Gs0, Xs, N0, Env, N1, FuncTrees, Gs1),
	goals_conditions(Gs1, Xs, Env, Conds, Gs),
	sort(FuncTrees, SFuncTrees),
	sort(Conds, SConds),
	conds_collapse(SConds, CConds),
	func_trees_collapse(SFuncTrees, CFuncTrees),
	count_literals(CConds, CFuncTrees, N1, N),
	Xs = [X|_],
	T = t(CConds,CFuncTrees,X).

goal_in_predicate_tree(Pred, Goal) :-
	member(Cl, Pred),
	goal_in_clause_tree(Cl, Goal).

goal_in_clause_tree(tcl(_,_,Ts), Goal) :-
	member(T, Ts),
	goal_in_tree(T, Goal).

goal_in_tree(t(Conds,_FuncTrees,_), Goal) :-
	member(Goal,  Conds).
goal_in_tree(t(_Conds,FuncTrees,_), Goal) :-
	member(_Func-T, FuncTrees),
	goal_in_tree(T, Goal).

goal_to_func_ancs(tgoal(G, AncsL), G/1, AncsL).

goal_to_name_func_ancs(tgoal(G, AncsL), G,  G/1, AncsL).

% atoms_unaries([], _, []).
% atoms_unaries([A|As], X, [AX|AXs]) :-
% 	AX =.. [A,X],
% 	atoms_unaries(As, AXs).

role_type(role, 1, 2).
role_type(irole, 2, 1).

goals_func_trees(Gs0, Xs, N0, Env, N, FuncTrees, Gs) :-
	Xs = [X|_],
	role_type(Inv, HeadANo, OtherANo),
	select(goal(G,_), Gs0, Gs1),
	functor(G, Func, 2),
	arg(HeadANo, G, U), U == X, !,
	arg(OtherANo, G, Y),
	FuncTrees = [Func/Inv-T|FuncTrees0],
	(   nthchkeq(Y, Xs, 0, VarDist) ->
	    T = t([tgoal(vareq(VarDist),[])],[],Y), Gs2 = Gs1, N1 = N0
	;   goals_to_tree(Gs1, [Y|Xs], N0, Env, N1, T, Gs2)
	),
	goals_func_trees(Gs2, Xs, N1, Env, N, FuncTrees0, Gs).
goals_func_trees(Gs0, _, N, _Env, N, [], Gs0).

goals_conditions(Gs0, Xs, Env, Conds, Gs) :-
	Xs = [X|_],
	select(goal(G,Ancs0), Gs0, Gs1), 
	  G =.. [FuncName,U], U == X, !,
	Conds = [tgoal(FuncName,Ancs)|Conds0],
	(   Ancs0 = abox -> Ancs = abox
	;   FuncName = '$checkanc' -> Ancs = Ancs0
	;   Env = env(Dictionary,_,_),
	    cond_ancestors(FuncName/1, Ancs0, Dictionary, Ancs)
	),
	goals_conditions(Gs1, Xs, Env, Conds0, Gs).
goals_conditions(Gs, _, _Env, [], Gs).

cond_ancestors(Func, Ancs0, Dictionary, Ancs) :-
	dict_memberchk(pr(Func,_,PData), Dictionary),
	cond_ancestors1(Ancs0, PData, Ancs).

cond_ancestors1(Ancs0, pdata(_,_,_,ADep), Ancs) :-
	retain_only_those_with_functor_in(Ancs0, ADep, Ancs).

func_trees_collapse([], []).
func_trees_collapse([FT|FTs], CFTs) :-
	FT = IFunc-T,
	func_trees_collapse(FTs, IFunc, [T], CFTs).

func_trees_collapse([FT1|FTs], IFunc, Ts, CFTs) :-
	FT1 = IFunc-T, !, 
	func_trees_collapse(FTs, IFunc, [T|Ts], CFTs).
func_trees_collapse(FTs, IFunc, Ts0, CFTs) :-
	trees_collapse(Ts0, Ts),
	add_trees(Ts, IFunc, CFTs0, CFTs),
	(   FTs == [] -> CFTs0 = []
	;   FTs = [IFunc1-T1|FTs1],
	    func_trees_collapse(FTs1, IFunc1, [T1], CFTs0)
	).

trees_collapse([], []).
trees_collapse([T|Ts0], CTs) :-
        remove_subsumers_and_check_if_subsumer(Ts0, T, Ts1, Res),
	(   Res = not_a_subsumer ->
	    CTs = [T|CTs1],
	    trees_collapse(Ts1, CTs1)
	;   Res = itself_a_subsumer ->
	    trees_collapse(Ts0, CTs)
	).

remove_subsumers_and_check_if_subsumer([], _T, [], not_a_subsumer).
remove_subsumers_and_check_if_subsumer([T2|Ts0], T1, Ts, Res) :-
	(   subsumer_tree_of(T1, T2) ->
	    Res = itself_a_subsumer
	;   subsumer_tree_of(T2, T1) ->
	    remove_subsumers_and_check_if_subsumer(Ts0, T1, Ts, Res)
	;   Ts = [T2|Ts1],
	    remove_subsumers_and_check_if_subsumer(Ts0, T1, Ts1, Res)
	).

subsumer_tree_of(T1, T2) :-
	ord_subtree(T1, T2).

add_trees([], _IFunc, CFTs0, CFTs0).
add_trees([T|Ts], IFunc, CFTs0, [IFunc-T|CFTs1]) :-
	add_trees(Ts, IFunc, CFTs0, CFTs1).

conds_collapse([], []).
conds_collapse([Cond|Conds], CConds) :-
	Cond = tgoal(Func,Ancs),
	conds_collapse(Conds, Func, [Ancs], CConds).

conds_collapse([Cond1|Conds], Func, AncsL, CConds) :-
	Cond1 = tgoal(Func,Ancs), !,
	conds_collapse(Conds, Func, [Ancs|AncsL], CConds).
conds_collapse(Conds, Func, AncsL, [tgoal(Func,CAncsL)|CConds]) :-
	ancestors_collapse(AncsL, CAncsL),
	(   Conds == [] -> CConds = []
	;   Conds = [tgoal(Func1,Ancs1)|Conds1],
	    conds_collapse(Conds1, Func1, [Ancs1], CConds)
	).

pair_with_lengths([], []).
pair_with_lengths([A|As], [L-A|LAs]) :-
	(   length(A, L) -> true
	;   A == abox -> L = -1
	;   A == failure -> L = 0
	),
	pair_with_lengths(As, LAs).

ancestors_collapse(AncsL0, CAncsL) :-
	sort(AncsL0, AncsL1),
	AncsL1 = [_,_|_], !, 
	pair_with_lengths(AncsL1, AncsL2),
	sort(AncsL2, AncsL3),
	unzip2(AncsL3, AncsL4),
	AncsL4 = [Ancs|AncsL5],
	remove_ancs_subsumers(Ancs, AncsL5, CAncsL).
ancestors_collapse(AncsL, AncsL).

remove_ancs_subsumers(Ancs, [], AncsL) :- !,
	AncsL = [Ancs].
remove_ancs_subsumers(abox, [Ancs|AncsL0], AncsL) :-
	remove_ancs_subsumers(Ancs, AncsL0, AncsL).
remove_ancs_subsumers(failure, _, AncsL) :-
	AncsL = [failure].
remove_ancs_subsumers(Ancs, AncsL0, AncsL) :-
	remove_supersets([Ancs|AncsL0], AncsL).

remove_supersets([], []).
remove_supersets([Ancs|AncsL0], [Ancs|CAncsL]) :-
	remove_supersets1(AncsL0, Ancs, AncsL1),
	remove_supersets(AncsL1, CAncsL).

remove_supersets1([], _Ancs, []).
remove_supersets1([Ancs0|AncsL0], Ancs, AncsL) :-
	ord_subset(Ancs, Ancs0), !,
	remove_supersets1(AncsL0, Ancs, AncsL).
remove_supersets1([Ancs0|AncsL0], Ancs, [Ancs0|AncsL]) :-
	remove_supersets1(AncsL0, Ancs, AncsL).

collapsible_ancs(abox, _Ancs2, A) :- !,
	A = abox.
collapsible_ancs(_Ancs1, abox, A) :- !,
	A = abox.
collapsible_ancs(Ancs, Ancs, A) :-
	!, A = Ancs.
collapsible_ancs(Ancs1, Ancs2, A) :-
	sort(Ancs1, A1),
	sort(Ancs2, A2),
	(   ord_subset(A1, A2) -> A = Ancs1
	;   ord_subset(A2, A1), A = Ancs2
	).




%---------------------------------------------------------------------------
% From SICStus library(ordset)
%   ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.


% Analogous to ord_subset, but for clause-trees.

ord_subtree(t(C1,FT1,_), t(C2,FT2,_)) :-
	ord_sub_conds(C1, C2),
	ord_sub_functree(FT1, FT2).

ord_sub_conds([], _).
ord_sub_conds([tgoal(Func1,AncsL1)|Tail1], [tgoal(Func2,AncsL2)|Tail2]) :-
	compare(Order, Func1, Func2),
	ord_sub_conds(Order, Func1, AncsL1, AncsL2, Tail1, Tail2).

ord_sub_conds(=, _, AncsL1, AncsL2, Tail1, Tail2) :-
	ancslist_subsumes(AncsL1, AncsL2),
	ord_sub_conds(Tail1, Tail2).
ord_sub_conds(>, Func1, AncsL1, _, Tail1, [tgoal(Func2,AncsL2)|Tail2]) :-
	compare(Order, Func1, Func2),
	ord_sub_conds(Order, Func1, AncsL1, AncsL2, Tail1, Tail2).

ancslist_subsumes([Ancs1|AncsL1], AncsL2) :-
	member(Ancs2, AncsL2),
	ancs_subsumes(Ancs1, Ancs2), !,
	ancslist_subsumes(AncsL1, AncsL2).
ancslist_subsumes([], _).

ancs_subsumes(Ancs, Ancs) :- !.
ancs_subsumes(Ancs1, Ancs2) :-
	ord_subset(Ancs2, Ancs1).

ord_sub_functree([], _).
ord_sub_functree([Func1-Tree1|Tail1], [Func2-Tree2|Tail2]) :-
	compare(Order, Func1, Func2),
	ord_sub_functree(Order, Func1, Tree1, Tree2, Tail1, Tail2).

ord_sub_functree(=, _, Tree1, Tree2, Tail1, Tail2) :-
	ord_subtree(Tree1, Tree2),
	ord_sub_functree(Tail1, Tail2).
ord_sub_functree(>, Func1, Tree1, _, Tail1, [Func2-Tree2|Tail2]) :-
	compare(Order, Func1, Func2),
	ord_sub_functree(Order, Func1, Tree1, Tree2, Tail1, Tail2).


%   ord_member(+Elt, +Set)
%   is true when Elt is a member of Set.  Suggested by Mark Johnson.

% ord_member(X, [E|Es]) :-
%         compare(C, X, E),
%         ord_member(C, X, Es).

% ord_member(=, _X, _Es).
% ord_member(>, X, [E|Es]) :-
%         compare(C, X, E),
%         ord_member(C, X, Es).

% ---------------------------------------------------------------------------

%   Based on the above ord_member

ord_key_member(X-XV, [E-EV|EEVs]) :-
        compare(C, X, E),
        ord_key_member(C, X, XV, EV, EEVs).

ord_key_member(=, _X, XV, XV, _EEVs).
ord_key_member(>, X, XV, _EV, [E-EV|EEVs]) :-
        compare(C, X, E),
        ord_key_member(C, X, XV, EV, EEVs).

% edge(+,+,+).
edge(From, To, UGraph) :-
	ord_key_member(From-Ends, UGraph),
	ord_member(To, Ends).

% nd_edge(?,?,+).
nd_edge(From, To, UGraph) :-
	member(From-Ends, UGraph),
	member(To, Ends).

edge_endpoints(From, Ends, UGraph) :-
	ord_key_member(From-Ends, UGraph).


unzip1([], []).
unzip1([X-_|XYs], [X|Xs]) :-
	unzip1(XYs, Xs).	

unzip2([], []).
unzip2([_-Y|XYs], [Y|Ys]) :-
	unzip2(XYs, Ys).	

opt_member(Opts, _Default, Opt) :-
	member(Opt, Opts), !.
opt_member(_, Default, Opt) :-
	arg(1, Opt, OVar),
	OVar = Default.

nthchkeq(X, [Y|_], N, N) :- X == Y, !.
nthchkeq(X, [_|L], N0, N) :-
	N1 is N0+1,
	nthchkeq(X, L, N1, N).

my_error(Format, Args) :-
	write(user_error, '! '),
	format(user_error, Format, Args),
	(   bb_get(context, File-Opt) ->
	    format(user_error, ', file ~w, opts ~w', [File,Opt])
	;   true
	),	    
	nl(user_error).

