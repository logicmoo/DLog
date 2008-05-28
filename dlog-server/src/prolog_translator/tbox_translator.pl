% (c) BME
% v0.2
% todo:
% - unfolding
% - not_ -> not/1
% - inv_ -> inv/1
:- module(prolog_translator,[tbox2prolog/3]).

:- use_module(library(lists), [append/3, member/2, last/2, select/3]).
:- use_module(library(ugraphs), [vertices_edges_to_ugraph/3]).

:- use_module('../core/config').
:- target(sicstus) -> 
		use_module(library(lists),[memberchk/2]),
		use_module(library(terms), [term_variables_bag/2]),
		use_module(library(ugraphs), [reduce/2])
		; true.
	 
:- target(swi) -> 
		use_module(library(listing), [portray_clause/1]), 
		use_module(prolog_translator_swi_tools, [term_variables_bag/2, reduce/2, bb_put/2, bb_get/2])
		; true.

:- use_module(transforming_tools, [headwrite/1, neg/2, contra/2, cls_to_neglist/2, body_to_list/2]).

:- dynamic 
	predicate/2,
	goal/2,
	orphan/2,
	atomic_predicate/2,
	atomic_like_predicate/2,
	inverse/2,
	hierarchy/2, % ket fo szinonima kozti hierarchia
	roleeq/2, % egy szerep fo szinonimaja
	role_component/1, % a szerepek kozti ekvivalenciaosztalyok
	symmetric/1. % szimmetrikus komponensek

:- thread_local
	predicate/2,
	goal/2,
	orphan/2,
	orphan_like/2,
	atomic_predicate/2,
	atomic_like_predicate/2,
	inverse/2,
	hierarchy/2, % ket fo szinonima kozti hierarchia
	roleeq/2, % egy szerep fo szinonimaja
	role_component/1, % a szerepek kozti ekvivalenciaosztalyok
	symmetric/1. % szimmetrikus komponensek

counter(loop).
counter(ancres).
counter(orphancres).

	
% tbox2prolog(+KB, + Signature)
% PFile is a filename where the transformed
% Prolog clauses are written
%
% Available options:
% statistics(no) : [yes, no] generates code that collects runtime statistics
% orphan(priority) : [normal, priority] whether orphan calls are considered to be normal concept calls
% decompose(yes) : [yes, no] whether to decompose the bodies into SCCs
% removed: allinone(no) : [yes, no] whether to generate all-in-one Prolog code
% indexing(yes) : [yes, no] whether to generate inverses for roles for efficient indexes
% projection(yes): [no, yes] whether to generate projection goals
% preprocessing(yes): [yes, no] whether to filter out clauses with orphan calls if possible+query predicates
% ground_optim(yes): [yes, no] whether to use ground goal optimization
% filter_duplicates(no) : [yes, no] whether to filter duplicates
tbox2prolog(URI, tbox(TBox, _IBox, HBox), abox(Signature)) :-
	init(URI),
	dl_preds(TBox, Preds),
	preprocessing(Preds, Signature, DepGraph), % asserts orphan/2, atomic_predicate/2, atomic_like_predicate/2
	processed_hbox(HBox, Signature),
	headwrite('Transformed TBox clauses'),
	transformed_kb(DepGraph, Signature).


transformed_kb(DepGraph, Signature) :-
	transform(DepGraph, DepGraph, Signature),
	headwrite('HI predicates'),
	generated_HI(Signature),
	generated_atomic,
	write_orphans,
	write_statistics.

generated_atomic :-
	(
	  atomic_predicate(_, 1) ->
	  headwrite('Transformed Atomic clauses')
	;
	  true
	),
	generated_atomic0.

generated_atomic0 :-
	atomic_predicate(A, 1),
	format('\% Atomic predicate ~p~n',[A/1]),
	write_atomic_predicate(A),
	fail.
generated_atomic0.

write_atomic_predicate(Pred) :-
	functor(Head, Pred, 1),
	arg(1, Head, _A),
	bb_get(aboxmodule, Module),
	portray_clause((Head :- Module:Head)),
	nl.

%%%%%%%%%%%%%%%%%%%%%%%%%
% Init
%%%%%%%%%%%%%%%%%%%%%%%%%

init(URI) :-
	bb_put(uri, URI),
	abox_module_name(URI, ABoxModule),
	bb_put(aboxmodule, ABoxModule),
	retractall(predicate(_,_)),
	retractall(goal(_,_)),
	retractall(orphan(_,_)),
	retractall(orphan_like(_,_)),
	retractall(atomic_predicate(_,_)),
	retractall(atomic_like_predicate(_,_)),
	retractall(inverse(_,_)),
	retractall(hierarchy(_,_)), % ket fo szinonima kozti hierarchia
	retractall(roleeq(_,_)), % egy szerep fo szinonimaja
	retractall(role_component(_)), % a szerepek kozti ekvivalenciaosztalyok
	retractall(symmetric(_)). % szimmetrikus komponensek


% dl_preds(+TBox, -Preds)
% Preds is a list of Key-Value pairs, where Key is a functor, Value
% is the list of clauses belong to the given functor represented
% as Body-Head pairs
dl_preds(TBox, Preds) :-
	bagof(Functor-Clauses,
	      setof(Body-Head,
		    Name^Arity^(tunnel(TBox, Name, Arity, Head, Body), Functor = Name/Arity),
		    Clauses),
	      Preds).

% current_predicate(Name, dl:Head), clause(dl:Head, Body),
tunnel(TBox, Name, Arity, Head, Body) :-
	member(C, TBox),
	cls_to_neglist(C, P),
	contra(P, Contras),
	member((Head :- Body), Contras),
	functor(Head, Name, Arity),
	Arity \== 2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HBox feldolgoz�sa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% felassertalt termek:
% role_component/1
% roleeq/2
% hierarchy/2
processed_hbox(HBox, Signature) :-
	hbox_edges(HBox, Edges),
	graph_nodes(Edges, Signature, Nodes),
	vertices_edges_to_ugraph(Nodes, Edges, Graph),
	reduce(Graph, Scc),
	asserted_hierarchy_info(Scc),
	asserted_inverses(Scc).

graph_nodes(Edges, Signature, Nodes) :-
	setof(Node, graph_nodes0(Edges, Signature, Node), Nodes0),
	close_inv(Nodes0, Nodes).

graph_nodes0(Edges, Signature, Node) :-
	(
	  member(_R1-Node, Edges)
	;
	  member(Node/2, Signature)
	;
	  atomic_predicate(Node, 2) % grr
	),
	\+ atom_concat('inv_', _, Node).

close_inv([], []).
close_inv([R|Rs], [R, InvR|InvRs]) :-
	atom_concat('inv_', R, InvR),
	close_inv(Rs, InvRs).

hbox_edges(HBox, Edges) :-
	hbox_edges0(HBox, Edges0),
	sort(Edges0, Edges). % FIXME: kell ez?

hbox_edges0([], []).
hbox_edges0([subrole(arole(R1),arole(R2))|Rs], [R1-R2, IR1-IR2|Es]) :-
	abox_inverse_name(R1, IR1),
	abox_inverse_name(R2, IR2),
	hbox_edges0(Rs, Es).

asserted_hierarchy_info([]).
asserted_hierarchy_info([[P|Rs]-Es|Cs]) :-
	assert(role_component([P|Rs])), % egy komponens
	asserted_synonims([P|Rs], P), % P a kituntetett elem
	asserted_hierarchies(Es, P),
	asserted_hierarchy_info(Cs).

asserted_synonims([], _).
asserted_synonims([S|Ss], P) :-
	assert(roleeq(S, P)),
	asserted_synonims(Ss, P).

asserted_hierarchies([], _).
asserted_hierarchies([[E|_]|Cs], P) :-
	assert(hierarchy(E, P)),
	asserted_hierarchies(Cs, P).

asserted_inverses([]).
asserted_inverses([[P|Rs]-Es|Cs]) :-
	abox_inverse_name(P, InvP),
	component_boss([[P|Rs]-Es|Cs], InvP, Rest0, Boss),
	(
	  P == Boss ->
	  assert(symmetric([P|Rs])),
	  Rest = Rest0
	;
	  Rest0 = [_|Rest]
	),
	assert(inverse(P, Boss)), % kialakul a paros graf...
	asserted_inverses(Rest).

component_boss([[P|Rs]-_Es|Cs], Name, Rest, Boss) :-
	memberchk(Name, [P|Rs]), !,
	Boss = P,
	Rest = Cs.
component_boss([[P|Rs]-Es|Cs], Name, [[P|Rs]-Es|Rest], Boss) :-
	component_boss(Cs, Name, Rest, Boss).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Preprocessing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
preprocessing(Preds, Signature, Final_NPreds) :-
	first_phase(Preds, Signature), % kezdetleges orphan/2,  atomic/2 besorolasok
	fixpoint(Preds, Signature, NPreds), % atomic_like, uj atomic, uj orphan besorolasok
	recalculate_orphans(NPreds, Signature),
	calculate_orphan_likes(NPreds, Signature),
	findall(N/A, orphan_like(N, A), OL),
	orphan_like_preds(OL, OLPreds),
	append(NPreds, OLPreds, Final_NPreds). 
	

orphan_like_preds([], []).
orphan_like_preds([N/A|Ss], [N/A-[]|Ss0]) :-
	orphan_like_preds(Ss, Ss0).

calculate_orphan_likes(Preds, Signature) :-
	calculate_orphane_likes0(Signature, Preds).

calculate_orphane_likes0([], _).
calculate_orphane_likes0([Name/Arity|Ss], Preds) :-
	(
	 Arity == 2 -> 
	 true
	;
	 functor(G, Name, Arity),
	 neg(G, NG),
	 functor(NG, NGName, NGArity),
	 \+ predicate(Name, Arity), path(NGName/NGArity, Name/Arity, [], Preds) -> % ha af
  	 safe_assert(orphan_like(Name, Arity))
	;
	 true
	),
	calculate_orphane_likes0(Ss, Preds).

recalculate_orphans(Preds, Signature) :-
	retractall(orphan(_,_)),
	retractall(predicate(_,_)),
	retractall(goal(_,_)),
	first_phase0(Preds),
	orphane_clauses(Signature).	

fixpoint(Preds, Signature, NPreds) :-
	init_iteration,
	iterate(Preds, Preds, Signature, NPreds0),
	(
	  bb_get(fixpoint, yes) ->
	  NPreds = NPreds0
	;
	  fixpoint(NPreds0, Signature, NPreds)
	).

init_iteration :-
	bb_put(fixpoint, yes).

iterate([], _, _, []).
iterate([Name/Arity-Clauses|Preds], OrigGraph, Signature, NGraph0) :-
	(
	  \+ query_predicate(Name, Arity) ->
	  (
	    atomic_like_pred(Clauses, Name/Arity, OrigGraph) ->
	    bb_put(fixpoint, no),
	    safe_assert(atomic_like_predicate(Name, Arity)),
	    NGraph0 = [Name/Arity-Clauses|NGraph]
	  ;
	    orphan_filter(Clauses, Name/Arity, OrigGraph, RClauses) ->
	    (
	      RClauses == [] -> % all TBox clauses of the predicate are eliminated
	      (
	       \+ memberchk(Name/Arity, Signature) -> % there are no ABox clauses either
	       safe_assert(orphan(Name, Arity))
	      ;
	       functor(G, Name, Arity),
	       neg(G, NG),
	       functor(NG, NGName, NGArity),
	       \+ path(NGName/NGArity, Name/Arity, [], OrigGraph) ->
	       safe_assert(atomic_predicate(Name, Arity)) % Name/Arity has become atomic
	      ;
	       true
      	      ),
	      NGraph = NGraph0
	    ;
	      NGraph0 = [Name/Arity-RClauses|NGraph]
	    )
	  )
	;
	  NGraph0 = [Name/Arity-Clauses|NGraph] % query predicates are not considered again
	),
	iterate(Preds, OrigGraph, Signature, NGraph).

% Name/Arity minden kl�z�ra:
% egy kl�zban vannak �rva h�vasok (A1..An)
% ha ezek k�z�l b�rmelyik Ai-re igaz, hogy 
% (1) (Ai egy atomi predikatum neg�ltja vagy)
% (2) Ai neg�ltj�b�l nem lehet eljutni Name/Arity-be
% --> ez a kl�z elhagyhat�
orphan_filter(A, B, C, D) :-
	env_parameter(preprocessing, yes), !,
	real_orphan_filter(A, B, C, D).
orphan_filter(A, _B, _C, A).

real_orphan_filter([], _, _, []).
real_orphan_filter([Body-_Head|Cs], Name/Arity, OrigGraph, RClauses) :-
	body_to_list(Body, BodyList),
	orphan_filter0(BodyList, Name/Arity, [], OrigGraph), !,
	bb_put(fixpoint, no),
	real_orphan_filter(Cs, Name/Arity, OrigGraph, RClauses).
real_orphan_filter([Body-Head|Cs], Name/Arity, OrigGraph, [Body-Head|RClauses]) :-
	real_orphan_filter(Cs, Name/Arity, OrigGraph, RClauses).


% igaz, ha [G|Gs] g�lok k�z�tt van biztosan hamis
% �rva h�v�s vagy k�t k�l�nb�z� funktor� �rva h�v�s
orphan_filter0([G|Gs], Name/Arity, Gy, OrigGraph) :-
	functor(G, GName, GArity),
	once(orphan(GName, GArity)), !,
	neg(G, NG),
	functor(NG, NGName, NGArity),
	(
	  \+ path(NGName/NGArity, Name/Arity, [], OrigGraph)->
	  true
	;
	  Gy == [] ->
	  orphan_filter0(Gs, Name/Arity, [GName], OrigGraph)
	;
	  Gy = [POName], POName \== GName ->
	  true
	;
	  orphan_filter0(Gs, Name/Arity, Gy, OrigGraph)
	).
orphan_filter0([_G|Gs], Name/Arity, Gy, OrigGraph) :-
	orphan_filter0(Gs, Name/Arity, Gy, OrigGraph).

% succeeds if there is a path from Name1/Arity1 to Name2/Arity2
% in graph Graph
path(Name1/Arity1, Name2/Arity2, Road, Graph) :-
	(
	  Name1 == Name2, Arity1 == Arity2 -> % from A to A there is path
	  true
	;
	  memberchk(Name1/Arity1-Clauses, Graph),
	  referenced(Clauses, Name2/Arity2, [Name1/Arity1|Road], Graph)
	).

referenced([Body-_Head|Cs], Name/Arity, Road, Graph) :-
	body_to_list(Body, BodyList),
	(
	  referenced0(BodyList, Name/Arity, Road, Graph)->
	  true
	;
	  referenced(Cs, Name/Arity, Road, Graph)
	).

referenced0([G|Gs], Name/Arity, Road, Graph) :-
	functor(G, GName, GArity),
	(
	  GName==Name, GArity==Arity ->
	  true
	;
	  atomic_predicate(GName, GArity) ->
	  referenced0(Gs, Name/Arity, Road, Graph)
	;
	  \+memberchk(GName/GArity, Road), path(GName/GArity, Name/Arity, Road, Graph) ->
	  true
	;
	  referenced0(Gs, Name/Arity, Road, Graph)
	).

first_phase(Preds, Signature) :-
	first_phase0(Preds),
	first_atomic0(Signature, Preds),
	orphane_clauses(Signature).

first_atomic0([], _).
first_atomic0([Name/Arity|Ss], Preds) :-
	(
	  Arity == 2 -> % bin�ris mindenk�ppen atomi, FIXME ha ez nem j�
	  safe_assert(atomic_predicate(Name, Arity))
	;
	 functor(G, Name, Arity),
	 neg(G, NG),
	 functor(NG, NGName, NGArity),
	 \+ predicate(Name, Arity), \+ path(NGName/NGArity, Name/Arity, [], Preds) -> % nem af, 2008. 05. 04.
  	 safe_assert(atomic_predicate(Name, Arity))
	;
	  true
	),
	first_atomic0(Ss, Preds).

first_phase0([]).
first_phase0([Name/Arity-Clauses|Preds]) :-
	safe_assert(predicate(Name, Arity)),
	body_goals(Clauses),
	first_phase0(Preds).

orphane_clauses(Signature) :-
	A = 1, 
	findall(P/A, predicate(P, A), Preds),
	findall(G/A, goal(G, A), Goals),
	member(G/A, Goals),
	\+ memberchk(G/A, Preds),
	\+ memberchk(G/A, Signature), % nincs benne az abox-ban
	safe_assert(orphan(G, A)),
	fail.
orphane_clauses(_).

body_goals([]).
body_goals([Body-_Head|Cs]) :-
	body_goals0(Body),
	body_goals(Cs).

body_goals0(true) :- !.
body_goals0((G1,G2)) :- !,
	body_goals0(G1),
	body_goals0(G2).
body_goals0(G) :-
	functor(G, N, A),
	safe_assert(goal(N, A)),
	(
	  A == 2 ->
	  safe_assert(atomic_predicate(N, A))
	;
	  true
	).


% Name/Arity atomic_like, ha
% - a neg�ltj�b�l nem �rhet� el
% - kozvetve es kozvetlenul csak atomic_like vagy atomic c�lt hiv
atomic_like_pred(Clauses, Name/Arity, OrigGraph) :-
	functor(G, Name, Arity),
	neg(G, NG),
	functor(NG, NGName, NGArity),
	atomic_like_pred0(Clauses),
	\+ path(NGName/NGArity, Name/Arity, [], OrigGraph). % nem af

atomic_like_pred0([]).
atomic_like_pred0([Body-_Head|Cs]) :-
	body_to_list(Body, BodyList),
	atomic_like_body(BodyList),
	atomic_like_pred0(Cs).

atomic_like_body([]).
atomic_like_body([G|Gs]) :-
	functor(G, Name, Arity),
	query_predicate(Name, Arity),
	atomic_like_body(Gs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transforming all DL predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform([], _, _).
transform([Functor-Clauses|Preds], DepGraph, Signature) :-
	transformed_predicate(Clauses, Functor, DepGraph, Signature), nl, 
	transform(Preds, DepGraph, Signature).

write_orphans :-
	once(orphan(_,_)),
	headwrite('Transformed Orphan clauses'),
	orphan(G, Arity),
	format('\% Predicate ~p~n',[G]),
	transformed_orphan(G, Arity),
	fail.
write_orphans.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Final rearrangement of clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arranged_ancloop((G -> true), LoopAncPass, AL, ATBody) :-
	!, arranged_ancloop(G, LoopAncPass, AL, ATBody0),
	ATBody = (ATBody0 -> true).
arranged_ancloop((G1, G2), LoopAncPass, AL, ATBody) :-
	!,
	(
	  arranged_ancloop(G1, LoopAncPass, AL, ATBody0) ->
	  ATBody = (ATBody0, G2)
	;
	  arranged_ancloop(G2, LoopAncPass, AL, ATBody0),
	  ATBody = (G1, ATBody0)
	).
arranged_ancloop(G, LoopAncPass, AL, ATBody) :-
	term_variables_bag(G, Vars),
	varmemberchk(AL, Vars),
	ATBody = (LoopAncPass, G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transforming one single predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transformed_predicate(Clauses, Name/Arity, DepGraph, Signature) :-
	(
	  atomic_like_predicate(Name, Arity) -> % concepts referring only to query predicates
	  format('\% Atomic-like predicate ~p~n',[Name/Arity]),
	  transformed_nonatomic_predicate_entry(Clauses, Name, DepGraph, Signature)
	;
	  orphan_like(Name, Arity) -> 
	  format('\% Orphan-like predicate ~p~n',[Name/Arity]),
	 transformed_nonatomic_predicate_entry(Clauses, Name, DepGraph, Signature)
	;
	  % non-atomic concept predicates
	  format('\% Predicate ~p~n',[Name/Arity]),
	  transformed_nonatomic_predicate_entry(Clauses, Name, DepGraph, Signature)
	).


write_generated_clauses(GClauses, OGClauses) :-
	normal_predicates(GClauses, GClauses0),
	write_clauses(GClauses0),
	(
	  (env_parameter(projection, yes) ; env_parameter(ground_optim, yes)) ->
	  once_predicates(OGClauses, OGClauses0),
	  write_clauses(OGClauses0)
	;
	  true
	).
	
normal_predicates([], []).
normal_predicates([G|Gs], Goal) :-
	(
	  G == [] ->
	  normal_predicates(Gs, Goal)
	;
	  Goal = [G0|Gs1],
	  (
	    G = (Head :- Body)->
	    true
	  ;
	    Head = G,		% facts
	    Body = true
	  ),
	  % change the name
	  Head =.. [Name|Args],
	  atom_concat('normal_', Name, OnceName),
	  Head0 =.. [OnceName|Args],
	  G0 = (Head0 :- Body),
	  normal_predicates(Gs, Gs1)
	).
	  
once_predicates([], []).
once_predicates([G|Gs], Goal) :-
	(
	  G == [] ->
	  once_predicates(Gs, Goal)
	;
	  Goal = [G0|Gs1],
	  (
	    G = (Head :- Body)->
	    true
	  ;
	    Head = G,		%facts
	    Body = true
	  ),
	  % change the name
	  Head =.. [Name|Args],
	  atom_concat('once_', Name, OnceName),
	  Head0 =.. [OnceName|Args],
	  % append a cut to the bodies
	  body_to_list(Body, BodyList),
	  (
	    last(BodyList, LastGoal),
	    (LastGoal == fail ; LastGoal == !) -> % do not put cut after fail or cut
	    Body0 = Body
	  ;
	    append(BodyList, [!], BodyList0),
	    body_to_list(Body0, BodyList0)
	  ),
	  G0 = (Head0 :- Body0),
	  once_predicates(Gs, Gs1)
	).


write_clauses([]).
write_clauses([C|Cs]) :-
	write_clauses0([C|Cs]),
	nl.

write_clauses0([]).
write_clauses0([C|Cs]) :-
	(
	  C \== [] ->
	  portray_clause(C)
	;
	  true
	),
	write_clauses0(Cs).

helper_goal(Head0, Head1, Head2, Cat) :-
%	env_parameter(allinone, yes), !,
	helper_goal0(Head0, Head1, Head2, Cat).
%helper_goal(_, _, _, _).

helper_goal0(Head0, Head1, _Head2, Cat) :-
	(env_parameter(ground_optim, yes); env_parameter(projection, yes)), !,
	(
	  cat(normal, Cat) ->
	  transformed_head(Head1, St, Body),
	  HelperBody = Body
	;
	  HelperBody = Head1
	),
	portray_clause((Head0 :- init_state(St), HelperBody)),
	nl.
helper_goal0(Head0, _Head1, Head2, Cat) :-
	(
	  cat(normal, Cat) ->
	  transformed_head(Head2, St, Body),
	  HelperBody = Body
	;
	  HelperBody = Head2
	),
	portray_clause((Head0 :- init_state(St), HelperBody)),
	nl.

solution_branch(Goal, SolutionBranch) :-
	(env_parameter(projection, no); env_parameter(filter_duplicates, yes)), !,
	SolutionBranch = (call(Goal), dlog:safe_assert(Goal), fail).
solution_branch(Goal, SolutionBranch) :-
	SolutionBranch = (call(Goal), bb_get(solnum, S), S1 is S+1, bb_put(solnum, S1), fail).

fail_branch(T0, HeadVar, FailBranch) :-
	FailBranch = (statistics(runtime, [T1,_]),
			 T is T1-T0,
			 dlog:get_counts(CVs),
			 bb_get(solnum, S),
			 HeadVar = [solnum=S, runtime=T|CVs]
		     ).

  
choice_goal(Name, Projection, ChoiceHead, HeadVar, Cat) :-
	(env_parameter(ground_optim, yes);env_parameter(projection, yes)), !, 
	atom_concat('once_', Name, OncePredName),
	atom_concat('normal_', Name, NormalPredName),
	(
	  cat(normal, Cat) ->
	  NormalPredHead =.. [NormalPredName, HeadVar, AL],
	  OncePredHead =.. [OncePredName, HeadVar, AL],
	  transformed_head(ChoiceHead,  AL, ChoicePredHead)
	;
	  NormalPredHead =.. [NormalPredName, HeadVar],
	  OncePredHead =.. [OncePredName, HeadVar],
	  ChoicePredHead = ChoiceHead
	),
	(
	  env_parameter(projection, yes) ->
	  NormalBranch = (Projection, OncePredHead)
	;
	  NormalBranch = NormalPredHead
	),
	ChoicePredBody = ((nonvar(HeadVar)-> OncePredHead ; NormalBranch)),
	ChoicePred = (ChoicePredHead :- ChoicePredBody),
	portray_clause(ChoicePred), nl.
choice_goal(_Name, _Projection, _ChoiceHead, _HeadVar,  _Cat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Categorisating normal preds and clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% categorised_normal_predicate(+Name, +DepGraph, -Categorisation, -ClauseCategorisation)
% Categorisation is a list consting of 'normal', 'recursive', 'af'
% ClauseCategorisation is a list containing a list for each clause of Name/1
categorised_normal_predicate(Name, Clauses, DepGraph, Categorisation, ClauseCategorisation) :-
	Arity = 1,
	\+ atomic_like_predicate(Name, Arity), !,
	functor(Head, Name, Arity),
	neg(Head, NHead),
	functor(NHead, NHeadName, NHeadArity),
	clause_categorisation(Clauses, Name, Arity, NHeadName, NHeadArity, DepGraph, ClauseCategorisation),
	Categorisation = [normal|Categorisation0],
	(
	  loop_path(ClauseCategorisation) ->
	  Categorisation0 = [recursive|Categorisation1]
	;
	  Categorisation0 = Categorisation1
	),
	(
	  path(NHeadName/NHeadArity, Name/Arity, [], DepGraph) ->
	  Categorisation1 = [af]
	;
	  Categorisation1 = []
	).
categorised_normal_predicate(_Name, Clauses, _DepGraph, [], Dummy) :-
	dummy_categorisation(Clauses, Dummy).

clause_categorisation([], _, _, _, _, _, []).
clause_categorisation([Body-Head|Cs], Name, Arity, NHeadName, NHeadArity, DepGraph, [B|Bs]) :-
	real_clause_categorisation(Head, Body, Name, Arity, NHeadName, NHeadArity, DepGraph, B),
	clause_categorisation(Cs, Name, Arity, NHeadName, NHeadArity, DepGraph, Bs).
	
real_clause_categorisation(Head, Body, Name, Arity, NHeadName, NHeadArity, DepGraph, RCat) :-
	(
	  referenced([Body-Head], Name/Arity, [], DepGraph) ->  
	  RCat = [loopput|RCat1]
	;
	  RCat = RCat1
	),
	(
	  referenced([Body-Head], NHeadName/NHeadArity, [], DepGraph) ->
	  RCat1 = [ancput]
	;
	  RCat1 = []
	).

loop_path([C|_Cs]) :-
	memberchk(loopput, C), !.
loop_path([_C|Cs]) :-
	loop_path(Cs).

% for atomic_like predicates we create dummy categorisation
dummy_categorisation([], []).
dummy_categorisation([_Body-_Head|Cs], [[query]|Bs]) :-
	dummy_categorisation(Cs, Bs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transformations: binary (HI)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ha van indexeles
% r(A, B) :- abox:r(A, B).
% r(A, B) :- abox:idx_inv_r(A, B).
% r(A, B) :- abox:s(A, B).
% r(A, B) :- abox:idx_inv_s(A, B).
% s(A, B) :- r(A, B).

% indexeles nelkul
% r(A, B) :- abox:r(A, B).
% r(A, B) :- abox:inv_r(B, A).
% r(A, B) :- abox:s(A, B).
% r(A, B) :- abox:inv_s(B, A).
% s(A, B) :- r(A, B).

generated_HI(Signature) :-
	role_component([P|Rs]),
	component_goals(P, Rs, Signature, [PGoals, RGoals]),
	\+ (PGoals == [], RGoals == []),
	(
	  symmetric([P|Rs]) ->
	  format('\% Symmetric role ~p/2 and its synonims~n',[P])
	;
	  format('\% Role ~p/2 and its synonims~n',[P])
	),
	write_clauses(PGoals),
	write_clauses(RGoals),
	fail.
generated_HI(_Signature) :-
	hierarchy(R1, R2),
	format('\% Subrole of ~p/2 is ~p/2~n',[R1, R2]),
	write_hierarchy(R1, R2), nl,
	fail.
generated_HI(_Signature).


component_goals(P, Rs, Signature, [PGoals, RGoals]) :-
	write_primary_role([P|Rs], P, Signature, [], PGoals),
	write_component_rest(Rs, P, RGoals).

write_primary_role([], _P, _Signature, Gy, Gy).
write_primary_role([R|Rs], P, Signature, Gy, Cs) :-
	aboxed_role(P, R, Signature, Clauses),
	append(Gy, Clauses, NGy),
	write_primary_role(Rs, P, Signature, NGy, Cs).

aboxed_role(R1, R2, Signature, Clauses) :-
	env_parameter(indexing, yes), !,
	HeadR1 =.. [R1, A, B],
	HeadR2 =.. [R2, A, B],
	abox_inverse_name(R2, IR2),
	atom_concat('idx_', IR2, IIR2),
	IHeadR2 =.. [IIR2, A, B],
	aboxed_role0(R2, IR2, HeadR1, HeadR2, IHeadR2, Signature, Clauses).
aboxed_role(R1, R2, Signature, Clauses) :-
	HeadR1 =.. [R1, A, B],
	HeadR2 =.. [R2, A, B],
	abox_inverse_name(R2, IR2),
	IHeadR2 =.. [IR2, B, A],
	aboxed_role0(R2, IR2, HeadR1, HeadR2, IHeadR2, Signature, Clauses).

aboxed_role0(R1, R2, HeadR1, HeadR2, IHeadR2, Signature, Clauses) :-
	bb_get(aboxmodule, Module),
	(
	  memberchk(R1/2, Signature) ->
	  Clauses = [(HeadR1 :- Module:HeadR2)|Clauses0]
	;
	  Clauses = Clauses0
	),
	(
	  memberchk(R2/2, Signature) ->
	  Clauses0 = [(HeadR1 :- Module:IHeadR2)]
	;
	  Clauses0 = []
	).	

write_component_rest([], _P, []).
write_component_rest([R|Rs], P, [(HeadR :- HeadP)|Cs]) :-
	HeadP =.. [P, A, B],
	HeadR =.. [R, A, B],
	write_component_rest(Rs, P, Cs).

% r2 < r1 
% r1(A, B) :- r2(A, B).
write_hierarchy(R1, R2) :-
	env_parameter(indexing, yes), !,
	HeadR1 =.. [R1, A, B],
	HeadR2 =.. [R2, A, B],
	portray_clause((HeadR1 :- HeadR2)).
write_hierarchy(R1, R2) :-
	HeadR1 =.. [R1, A, B],
	HeadR2 =.. [R2, A, B],
	inverse(R2, _), !, 
	indexed_transformed_binary(HeadR1, [A], THR1),
	portray_clause((THR1 :- HeadR2)).

abox_inverse_name(R, IRole) :-
	atom_concat('inv_', IRole, R), !.
abox_inverse_name(R, IRole) :-
	atom_concat('inv_', R, IRole).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transformations: nonatomic (ALC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transformed_nonatomic_predicate_entry(Clauses, Name, DepGraph, Signature) :-
	PredHead =.. [Name, Var],
	(
	  env_parameter(projection, yes)->
	  (
	    global_projection(PredHead, Projection, Signature, DepGraph)->
	    bb_put(projection, success)
	  ;
	    bb_put(projection, failed),
	    asserta(env_user:projection(no)) % temporarily turn off projection
	  )
	;
	  bb_put(projection, notneeded)
	),
	categorised_normal_predicate(Name, Clauses, DepGraph, PCategorisation, CCategorisation),
	atom_concat('choice_', Name, ChoicePredName),
	ChoicePred =.. [ChoicePredName, Var],
	atom_concat('normal_', Name, NormalPredName),
	NormalPred =.. [NormalPredName, Var],
	transformed_nonatomic_predicate(Clauses, Name, GClauses, OGClauses, PCategorisation, CCategorisation, Signature),
	helper_goal(PredHead, ChoicePred, NormalPred, PCategorisation), % write helper predicate
	choice_goal(Name, Projection, ChoicePred, Var, PCategorisation), % write choice predicate
	write_generated_clauses(GClauses, OGClauses), % write abox and other clauses
	(
	  bb_get(projection, failed) ->
	  retractall(env_user:projection(no)) % switch back projection on
	;
	  true
	).

transformed_nonatomic_predicate(Clauses, Name, NClauses, [GClause1,GClause2|OGClauses], PCat, CCat, Signature) :-
	Head =.. [Name, HeadVar],
	(
	  looptest(Head, AL, PCat, LoopTest) -> % sikerul, ha recursive
	  GClause1 = (TransformedHead :- LoopTest)
	;
	  GClause1 = []	  
	),
	(
	  anctest(ancres, Head, AL, PCat, AncTest) -> % sikerul, ha af
	  GClause2 = (TransformedHead :- AncTest)
	;
	  GClause2 = []
	),
	(
	  cat(normal, PCat) -> 
	  transformed_head(Head, AL, TransformedHead)
	;
	  TransformedHead = Head	  
	),
	(
	  \+ memberchk(Name/1, Signature) ->
	  OGClauses = OGClauses0,
	  GClauses = GClauses0
	;
  	  ABoxCall =.. [Name, HeadVar],
	  bb_get(aboxmodule, Module),
	  ABoxGoal = (TransformedHead :- Module:ABoxCall),
	  OGClauses = [ABoxGoal|OGClauses0],
	  GClauses = [ABoxGoal|GClauses0]
	),
	% transforming the rest of the clauses
	transformed_concept_predicate(Clauses, GClauses0, CCat, OGClauses0), 
	(
	  env_parameter(projection, yes) ->
	  NClauses = [] % in case of projection there is no normal branch
	;
	  NClauses = [GClause1,GClause2|GClauses]
	).

cat(Category, Categories) :-
	memberchk(Category, Categories).


transformed_concept_predicate([], [], _, []).
transformed_concept_predicate([Body-Head|Cs], NClauses, [RCat|RCats], [OGClause|OGClauses]) :-
	arg(1, Head, HeadVar),
	(
	  env_parameter(projection, no)->
	  NClauses = [GClause|GClauses],
	  transformed_rule(Head, Body, HeadVar, [], RCat, GClause) % normal branch
	  ;
	  GClauses = NClauses % in case of projection(yes) there are only once_ clauses
	),
	transformed_rule(Head, Body, HeadVar, [HeadVar], RCat, OGClause), % once_ branch, ujrasorrendezes
	transformed_concept_predicate(Cs, GClauses, RCats, OGClauses).

transformed_rule(Head, Body, HeadVar, Vars, RCat, Clause) :-
	(
	  Vars == [] ->
	  Mode = normal % all components, but the first, are arrowed
	;
	  Mode = once % all components are arrowed, last one is cut
	),
	transformed_head(Head, AncestorList, THead),
	body_to_list(Body, BodyList),
	orphaned_goals(BodyList, Orphans, RBodyList, OVars), % collecting orphan calls
	wrapped_orphans(Orphans, Head, AncestorList, AL, WOrphans), % add ancestor list
	body_to_list(OrphanGoals, WOrphans),
	(
	  OrphanGoals == true -> % there are no orphan calls to move front
	  Rest = TBody0
	;
	  Rest = (FinalOrphanGoals, TBody0)
	),
	(
	  Mode == once ->
	  append(Vars, OVars, Vars1)
	;
	  Vars1 = Vars
	),
	(
	  cat(loopput, RCat), cat(ancput, RCat) ->
	  LoopAncPass = new_state(Head, AncestorList, AL)
	;
	  cat(loopput, RCat)->
	  LoopAncPass = new_loop(Head, AncestorList, AL)
	;
	  cat(ancput, RCat)->
	  LoopAncPass = new_anc(Head, AncestorList, AL)
	;
	  true
	),
	(
	  env_parameter(decompose,no)-> % simple rearrangement
	  rearranged_body(RBodyList, anc(AL, Head, AncestorList), HeadVar, Vars1, [], TBodyList0),
	  body_to_list(TBody1, TBodyList0)
	;
	  RBodyList \== [] ->
	  decomposed_body(RBodyList, [HeadVar], DBody), % initial decomposition
	  rearrange_decomposition(DBody, RDBody), % move components with binaries to the front
	  transformed_decomposed_bodies(RDBody, Mode, anc(AL, Head, AncestorList), HeadVar, Vars1, TBody1)
	;
	  TBody1 = true % if the body contains only orphan calls
	),
	(
	  varinbody(TBody1, AL), (cat(loopput, RCat) ; cat(ancput, RCat)) -> 
	  arranged_ancloop(TBody1, LoopAncPass, AL, ATBody), % putting LoopAncPass to the best place
	  TBody0 = ATBody,
	  FinalOrphanGoals = OrphanGoals
	;
	  varinbody(OrphanGoals, AL), (cat(loopput, RCat) ; cat(ancput, RCat)) ->
	  FinalOrphanGoals = (LoopAncPass, OrphanGoals),
	  TBody0 = TBody1
	;
	  AL = AncestorList,
	  FinalOrphanGoals = OrphanGoals,
	  TBody0 = TBody1  % special case: body does not contain reference to new anc/loop list
	),
	(
	  TBody0 == true ->
	  TBody = FinalOrphanGoals  % body contains only orphan calls
	;
	  TBody = Rest
	),
	(
	  cat(query, RCat) ->
	  FinalHead = Head
	;
	  FinalHead = THead
	),
	Clause = (FinalHead :- TBody).


transformed_decomposed_bodies(Bodies, Mode, Anc, HeadVar, Vars, RBody) :-
	transformed_bodies0(Bodies, Anc, HeadVar, Vars, TBodies),
	% felt�teles szerkezetek elhelyez�se
	cut_bodies(TBodies, Mode, RBody).

transformed_bodies0([], _Anc, _HeadVar, _Vars, []).
transformed_bodies0([B|Bs], Anc, HeadVar, Vars, [TB|TBs]) :-
	rearranged_body(B, Anc, HeadVar, Vars, [], TB), % TB helyesen z�r�jelezett, rendezett body-r�sz
	% HeadVar is instantiated from the second component onwards
	(
	  varmemberchk(HeadVar, Vars)->
	  Vars1 = Vars
	;
	  Vars1 = [HeadVar|Vars]
	),
	transformed_bodies0(Bs, Anc, HeadVar, Vars1, TBs).

transformed_head(Head, AL, THead) :-
	Head =.. [Name|Args],
	(
	  Args = [_,_]->
	  THead = Head
	;
	  append(Args, [AL], Args1),
	  THead =.. [Name|Args1]
	).

%%%%%%%%%%%%%%%
% Decomposition
%%%%%%%%%%%%%%%

% decomposed_body(+Body, +Vars, -DecomposedBody)
% Body is given is Prolog list notation
% Vars are the vars that must be ignored in the dependency graph
decomposed_body(Body, Vars, Bodies) :-
	goal_dependencies(Body, Vars, Edges, Nodes),
	vertices_edges_to_ugraph(Nodes, Edges, DepGraph),
	reduce(DepGraph, ReducedDepGraph),
	scc_vertices(ReducedDepGraph, Bodies).

scc_vertices([],[]).
scc_vertices([V-[]|Vs], [V|Bs]) :-
	scc_vertices(Vs, Bs).

goal_dependencies(Body, Vars, Edges, Nodes) :-
	goal_dependencies(Body, Vars, [], [], Edges, Nodes).

goal_dependencies([], _, GyE, GyN, GyE, GyN).
goal_dependencies([G|Gs], Vars, GyE, GyN, Edges, Nodes) :-
	goal_dependent(G, Gs, Vars, DGoals),
	(
	  DGoals == [] ->
	  append(GyN, [G], GyN1)
	;
	  GyN1 = GyN
	),
	append(GyE, DGoals, GyE1),
	goal_dependencies(Gs, Vars, GyE1, GyN1, Edges, Nodes).

goal_dependent(G, Gs, Vars, DGoals) :-
	term_variables_bag(G, GVars),
	vars_present(Gs, GVars, Vars, ConnectedGoals),
	graph_edges(ConnectedGoals, G, DGoals).

vars_present([], _, _, []).
vars_present([G|Gs], Vars, IVars, CGoals) :-
	term_variables_bag(G, GVars),
	(
	  common_variable(GVars, Vars, IVars)->
	  CGoals = [G|CGoals0]
	;
	  CGoals0 = CGoals
	),
	vars_present(Gs, Vars, IVars, CGoals0).
	
common_variable(L1, L2, IVars) :-
	member(V1, L1),
	\+ varmemberchk(V1, IVars),
	member(V2, L2),
	V1 == V2, !.

graph_edges([], _, []).
graph_edges([CG|CGs], G, [G-CG, CG-G|DGs]) :-
	   graph_edges(CGs, G, DGs).

% rearrange_decomposition(+Components, -RComponents)
% RComponents is the rearranged version of Components
% the first component with binaries
% with the smallest size is moved to the front
rearrange_decomposition(Comps, DComps) :-
	number_components(Comps, NComps),
	keysort(NComps, SNComps),
	unkey(SNComps, DComps).

number_components([], []).
number_components([C|Cs], [N-C|NCs]) :-
	number_component(C, N),
	number_components(Cs, NCs).

number_component(Component, Number) :-
	length(Component, Length),
	(
	  Length == 1, component_has_atomic(Component) -> % ground atomi predik�tum
	  Number = 0
	;
	  component_has_binary(Component)-> 
	  Number = Length
	;
	  Number = inf
	).

component_has_atomic([G]) :-
	functor(G, Name, Arity),
	query_predicate(Name, Arity).

component_has_binary([G|_Gs]) :-
	G \= _:_, % lehetnek abox:C alak� conceptek
	functor(G, _, 2), !.
component_has_binary([_G|Gs]) :-
	component_has_binary(Gs).

%%%%%%%%%%%%%%%%%%%%%%
% Ordering a component
%%%%%%%%%%%%%%%%%%%%%%

% rearranged_body(+BodyList, +Anc, +HeadVar, +Vars, +Gy, -RBody)
%
% RBody == list(Component)
% Component --> g(Goal)
%             | RBody0
%
% RBody0    --> list(g(Goal)|RBody)
%
% RBody0 egy olyan RBody aminek az els� eleme egy g(Goal)

rearranged_body([], _Anc, _HeadVar, _Vars, Gy, Gy).
rearranged_body([Goal|Goals], Anc, HeadVar, Vars, Gy, RBody) :-
	Anc = anc(AL, _, _),
	select_goal(SGoal, [Goal|Goals], HeadVar, Vars, RGoals), % selection function
	transformed_head(SGoal, AL, AncGoal0),
	functor(SGoal, Name, Arity),
	(
	  orphan(Name, Arity), env_parameter(projection, no) -> % in case of orphan calls we cannot assume instantiation
	  GVars = []
	;
	  term_variables_bag(SGoal, GVars)
	),
	wrapped_goal(SGoal, AncGoal0, GVars, Vars, Anc, AncGoal),
	append(GVars, Vars, Vars1),
	append(Gy, [AncGoal], Gy1),
	(
	  env_parameter(decompose,no) ->
	  rearranged_body(RGoals, Anc, HeadVar, Vars1, Gy1, RBody)
	;
	  % recursive decomposition
	  (
	    RGoals == [] ->	% no more goals
	    RBody = g(AncGoal)
	  ;
	    new_headvar(GVars, HeadVar, NHeadVar),
	    decomposed_body(RGoals, Vars1, DBody),
	    rearrange_decomposition(DBody, RDBody), % move components with binaries to the front	    
	    transformed_bodies0(RDBody, Anc, NHeadVar, Vars1, TBody),
	    RBody = [g(AncGoal)|TBody]
	  )
	).


new_headvar([V1,V2], Old, New) :-
	!,
	(
	  V1 == Old ->
	  New = V2
	;
	  New = V1
	).
new_headvar(_, Old, New) :-
	New = Old.

wrapped_goal(SGoal, Goal, GVars, AllVars, anc(AL, H, AL0), RGoal) :-
	functor(SGoal, Name, Arity),
	Goal =.. [_|Args],
	(
	  atomic_predicate(Name, Arity) ->
	  (
	    Arity == 1 ->
	    RGoal = SGoal
	  ;
	    indexed_transformed_binary(SGoal, AllVars, RGoal0),
	    RGoal = RGoal0
	  )
	;
	  atomic_like_predicate(Name, Arity) ->
	  SGoal =.. [_|SArgs],
	  (
	    env_parameter(projection, no), env_parameter(ground_optim, no) ->
	    atom_concat('normal_', Name, NormalName),
	    RGoal =.. [NormalName|SArgs]
	  ;
	    env_parameter(ground_optim, yes), ground_goal(GVars, AllVars) ->
	    atom_concat('once_', Name, OnceName),
	    RGoal =.. [OnceName|SArgs]
	  ;
	    atom_concat('choice_', Name, ChoiceName),
	    RGoal =.. [ChoiceName|SArgs]
	  )   
	;
	  orphan(Name, Arity) ->
	  functor(H, HeadName, _),
	  neg(SGoal, NSGoal),
	  functor(NSGoal, NSGoalName, _),
	  (
	    HeadName == NSGoalName ->
	    transformed_head(SGoal, AL, RGoal)
	  ;
	    transformed_head(SGoal, AL0, RGoal)
	  )
	;
	  env_parameter(projection, no), env_parameter(ground_optim, no) ->
	  atom_concat('normal_', Name, NormalName),
	  RGoal =.. [NormalName|Args]
	;
	  env_parameter(ground_optim, yes), ground_goal(GVars, AllVars) ->
	  atom_concat('once_', Name, OnceName),
	  RGoal =.. [OnceName|Args]
	;
	  atom_concat('choice_', Name, ChoiceName),
	  RGoal =.. [ChoiceName|Args]
	).

select_goal(Best, Goals, HeadVar, Vars, RGoals) :-
	numbered_goals(Goals, HeadVar, Vars, NGoals),
	keysort(NGoals, [_-Best|Rest]),
	unkey(Rest, RGoals).
	
numbered_goals([], _, _, []).
numbered_goals([G|Gs], HeadVar, Vars, [N-G|Gs0]) :-
	has_ranking(G, HeadVar, Vars, N),
	numbered_goals(Gs, HeadVar, Vars, Gs0).

% Ranking criteria:
% -2 : grounded property
% -1 : grounded atomic concept
%  0 : property with 1 unbounded variable
%  1 : property with 2 unbounded variables (at least one of them is the head variable)
%  2 : property with 2 unbounded variables (none of the is the head variable)
%  3 : non-grounded atomic concept
%  4 : grounded normal concept
%  5 : non-grounded normal concept
has_ranking(Goal, HeadVar, Vars, Distance) :-
	term_variables_bag(Goal, GVars),
	findall(V, (member(V, GVars),varmemberchk(V, Vars)), L),
	length(L, Distance0),
	functor(Goal, Name, Arity),
	(
	  query_predicate(Name, Arity) -> % binaries and special concepts
	  (
	    Arity = 2 -> % binaries
	    (
	      Distance0 == 2 -> % grounded properties
	      Distance = -2
	    ;
	      Distance0 == 1 -> % properties with one variable
	      Distance = 0
	    ;
	      varmemberchk(HeadVar, GVars) ->
	      Distance = 1 % properties with head variable 
	    ;
	      Distance = 2 % properties with two variables
	    )
	  ;
	    Distance is (-4)*Distance0+3 % atomic concepts are -1 or 3
	  )
	;
	  Distance is 5-Distance0 % normal concept calls are either 4 or 5
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Forming conditional structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cut_bodies(+Components, +Mode, -FinalGoal)
% Components is an RBody
% Mode can be either normal or once
% FinalGoal is the cut-included transformation of
% Components
cut_bodies(g(Goal), _, FinalGoal) :-
	!, FinalGoal = Goal.
cut_bodies(Components, Mode, FinalGoal) :-
	Mode == normal, !, 
	cut_bodies0(Components, [G|Gs]),
	arrow(Gs, Mode, AGs),
	flat_goals([G|AGs], FinalGoal).
cut_bodies(Components, Mode, FinalGoal) :-
	(Mode == once; Mode == special), !, 
	cut_bodies0(Components, CGoalList),
	arrow(CGoalList, Mode, AGoalList),
	flat_goals(AGoalList, FinalGoal).

cut_bodies0([], []).
cut_bodies0([B|Bs], [CB|CBs]) :-
	cut_bodies(B, normal, CB),
	cut_bodies0(Bs, CBs).

arrow([], _, []).
arrow([G|Gs], Mode, [AG|AGs]) :-
	(
	  Mode == once, Gs == [] ->
	  AG = (G, !) 
	;
	  arrow0(G, AG)
	),
	arrow(Gs, Mode, AGs).

arrow0((G1, G2), AG) :-
	!,
	AG = ((G1, G2) -> true).
arrow0(G, G).

%%%%%%%%%%%%%%%%%%
% other predicates
%%%%%%%%%%%%%%%%%%
orphaned_goals(Body, Orphans, Rest, OVars) :-
	env_parameter(orphan, normal), !,
	Orphans = [],
	Rest = Body,
	OVars = [].
orphaned_goals(Body, Orphans, Rest, OVars) :-
	orphaned_goals0(Body, Orphans, Rest, OVars).


wrapped_orphans([], _, _, _, []).
wrapped_orphans([O|Os], H, AL0, AL, [WO|WOs]) :-
	wrapped_goal(O, dummy(1), [], [], anc(AL, H, AL0), WO),	
	wrapped_orphans(Os, H, AL0, AL, WOs).



orphaned_goals0([], [], [], []).
orphaned_goals0([G|Gs], Orphans0, Rest0, OVars0) :-
	(
	  functor(G, Name, 1),
	  orphan(Name, 1)->
	  Orphans0 = [G|Orphans],
	  Rest = Rest0,
	  term_variables_bag(G, [Var]),
	  OVars0 = [Var|OVars]
	;
	  Orphans = Orphans0,
	  Rest0 = [G|Rest],
	  OVars = OVars0
	),
	orphaned_goals(Gs, Orphans, Rest, OVars).
	
ground_goal([], _).
ground_goal([V|Vs], Vs2) :-
	varmemberchk(V, Vs2),
	ground_goal(Vs, Vs2).

transformed_orphan(N, A) :-
	  functor(Head, N, A),
	  arg(1, Head, _Var),
	  transformed_head(Head, AL, TransformedHead),
	  anctest(orphancres, Head, AL, [af], AncTest),
	  portray_clause((TransformedHead :- AncTest, !)). % in case of orphan calls the is no backtrack


%%%%%%%%%%%%%%%%%%%%%
% loop and anc tests
%%%%%%%%%%%%%%%%%%%%%
looptest(Head, AL, Cat, LoopTest) :-
	cat(recursive, Cat),
	LoopTest0 = check_loop(Head, AL),
	(
	  env_parameter(statistics, yes) ->
	  LoopTest = (LoopTest0, !, dlog:count(loop), fail)
	;
	  LoopTest = (LoopTest0, !, fail)
	).

anctest(Counter, Head, AL, Cat, AncTest) :-
	cat(af, Cat),
	neg(Head, NHead),
	AncTest0 = check_anc(NHead, AL),
	(
	  env_parameter(statistics,yes) ->
	  AncTest =  (AncTest0, dlog:count(Counter))
	;
	  AncTest =  AncTest0
	).


%%%%%%%%%%%%%%%%%%%%%%%
% binary transformation
%%%%%%%%%%%%%%%%%%%%%%%
indexed_transformed_binary(Binary, Vars, TBinary) :-
	Binary =.. [Name, Arg1, Arg2],
	roleeq(Name, Boss),
	(
	  env_parameter(indexing, yes)->
	  indexed_transformed_binary1(Boss, Arg1, Arg2, Vars, TBinary)
	;
	  indexed_transformed_binary0(Boss, Arg1, Arg2, TBinary)
	).

indexed_transformed_binary0(Name, Arg1, Arg2, TBinary) :-
	(
	  inverse(Name, _) ->
	  Binary =.. [Name, Arg1, Arg2],
	  TBinary = Binary
	;
	  inverse(SName, Name) ->
	  TBinary =.. [SName, Arg2, Arg1]
	).

indexed_transformed_binary1(Name, Arg1, Arg2, Vars, TBinary) :-
	\+ varmemberchk(Arg1, Vars),
	varmemberchk(Arg2, Vars), !,
	(
	  inverse(Name, InvName) ->
	  TBinary =.. [InvName, Arg2, Arg1]
	;
	  inverse(InvName, Name) ->
	  TBinary =.. [InvName, Arg2, Arg1]
	).
indexed_transformed_binary1(Name, Arg1, Arg2, _Vars, Binary) :-
	Binary =.. [Name, Arg1, Arg2].

%%%%%%%%%%%%
% Projection
%%%%%%%%%%%%

% global_projection(+Pred, -Projection)
% Projection is a setof goal for Pred
global_projection(Pred, Projection, Signature, DepGraph) :-
	Pred =.. [Name, HeadVar],
	neg(Pred, NPred),
	functor(NPred, NName, _),
	% collect every binary and atomic concepts needed for the superset
	collect_binaries(Name, [], [NName], HeadVar, DepGraph, Signature, Binaries),
	% fails if predicate
	% (1) has clauses with atomic concepts only
	% (2) or if Binaries == [] (loop cases)
	component_has_binary(Binaries), 
	projection(Binaries, HeadVar, Projection).

% Kigy�jti egy list�ba a Name/1 predik�tumhoz tartoz�
% olyan c�lok list�j�t, amelyek r�szt vesznek a Name/1
% supersetj�nek �p�t�s�ben
% collect_binaries(+Name, +Path, +Orphans, +HeadVar, +DepGraph, +Signature, -Binaries)
collect_binaries(Name, Path, Orphans, HeadVar, Predicates, Signature, Binaries) :-
	% loop check
	\+ memberchk(Name, Path),
	memberchk(Name/1-Clauses, Predicates),
	collect_from_clauses(Clauses, [Name|Path], HeadVar, Predicates, Signature, Orphans, Binaries0),
	(
	  memberchk(Name/1, Signature) -> % we have abox clauses
	  ABoxGoal0 =.. [Name, HeadVar],
	  bb_get(aboxmodule, Module),
	  ABoxGoal = Module:ABoxGoal0,
	  (
	    % small optimization: if we have the same abox call
	    % at any branch we do not need it once more
	    goal_present(Binaries0, ABoxGoal) ->
	    Binaries = Binaries0
	    ;
	    Binaries = [[ABoxGoal]|Binaries0]
	  )
	;
	  Binaries = Binaries0
	).

collect_from_clauses([], _, _, _, _, _, []).
collect_from_clauses([Body-Head|Cs], Path, HeadVar, DepGraph, Signature, Orphans, Bs) :-
	arg(1, Head, LocalHeadVar),
	body_to_list(Body, BodyList),
	orphaned_goals0(BodyList, OGoals, _, _),
	(
	  orphan_ok(OGoals, Orphans) ->
	  binaries_with_headvar(BodyList, LocalHeadVar, Binaries0),
	  HeadVar = LocalHeadVar, % tricky
	  (
	    Binaries0 == [] ->	% clause with only non_atomic goals
	    memberchk(FGoal, BodyList), % selects any goal
	    functor(FGoal, Name, _),
	    neg(FGoal, NFGoal),
	    functor(NFGoal, NName, _),
	    (
	      collect_binaries(Name, Path, [NName|Orphans], LocalHeadVar, DepGraph, Signature, Binaries1), Binaries1 \== [] ->
	      open_ended(Binaries1, OpenList, EndVar),
	      Bs = OpenList, 
	      Binaries = EndVar
	    ;
	      Bs = Binaries % skip this clause as in runtime we would skip it as well
	    )
	  ;
	    Bs = [Binaries0|Binaries]
	  )
	;
	  Bs = Binaries % skip this clause
	),
	collect_from_clauses(Cs, Path, HeadVar, DepGraph, Signature, Orphans, Binaries).

orphan_ok([], _).
orphan_ok([O|Os], Orphans) :-
	orphan_ok0([O|Os], Orphans).

orphan_ok0([O|_Os], Orphans) :-
	functor(O, Name, _),
	memberchk(Name, Orphans), !.
orphan_ok0([_|Os], Orphans) :-
	orphan_ok0(Os, Orphans).
	

% projection(+Binaries, +Headvar, -Goal)
% Binaries == list(Component)
% Component == list(Goal)
% example:
% Binaries = [[has_child(Y, X), has_child(X, Z), has_child(X, Q), has_friend(X, W)], [clever(X)]]
% Headvar = X
% Goal = setof(V, A^B^C^(hasChild(A, V), has_child(V, B), has_friend(V, C); clever(V)), L), member(X, L).
projection(Binaries, HeadVar, Goal) :-
	setofgoal(Binaries, HeadVar, [], SetofGoal),
	quantifiers(SetofGoal, HeadVar, QuantifiedGoal),
	Goal = (setof(HeadVar, QuantifiedGoal, List), member(HeadVar, List)).

quantifiers(Binaries, HeadVar, Quantifiers) :-
	term_variables_bag(Binaries, Variables0),
	filtered_bag(Variables0, HeadVar, Variables1),
	quantified(Variables1, Binaries, Quantifiers).

filtered_bag([], _, []).
filtered_bag([V|Vs], HeadVar, Filtered) :-
	(
	  V == HeadVar ->
	  filtered_bag(Vs, HeadVar, Filtered)
	;
	  Filtered = [V|Vs0],
	  filtered_bag(Vs, HeadVar, Vs0)
	).
quantified([], Gy, Gy).
quantified([V|Vs], Gy, Quantifiers) :-
	(
	  Vs == [] ->
	  Quantifiers = V^Gy
	;
	  Gy1 = V^Gy,
	  quantified(Vs, Gy1, Quantifiers)
	).

setofgoal([], _HeadVar, _Majorants, Goal) :-
	!, Goal = true.
setofgoal([Conjunction|Cs], HeadVar, Majorants, Goal) :-
	is_majored(Conjunction, HeadVar, Majorants), !,
	% Conjunction is useless
	setofgoal(Cs, HeadVar, Majorants, Goal).
setofgoal([Conjunction|Cs], HeadVar, Majorants, Goal) :-
	setofgoal0(Conjunction, HeadVar, [], SortedConjunction),
	body_to_list(SortedConjunctionBody, SortedConjunction),
	(
	  Cs == [] ->
	  Goal = SortedConjunctionBody
	;
	  Goal = (SortedConjunctionBody ; Bs),
	  (
	    Conjunction = [G] ->
	    position_headvar(G, HeadVar, Name/P),
	    setofgoal(Cs, HeadVar, [Name/P|Majorants], Bs)
	  ;
	    setofgoal(Cs, HeadVar, Majorants, Bs)
	  )
	).

is_majored([G|_Gs], HeadVar, Majorants) :-
	position_headvar(G, HeadVar, Name/P),
	memberchk(Name/P, Majorants), !.
is_majored([_G|Gs], HeadVar, Majorants) :-
	is_majored(Gs, HeadVar, Majorants).

setofgoal0([], _, _, []).
setofgoal0([B|Bs], HeadVar, Gy, SetofGoal) :-
	B \= _:_,
	B =.. [Name, Var1, Var2], !, 
	position([Var1, Var2], HeadVar, P),
	(
	  memberchk(Name/P, Gy) ->
	  setofgoal(Bs, HeadVar, Gy, SetofGoal)
	;
	  indexed_transformed_binary(B, [HeadVar], TB),
	  SetofGoal = [TB|SG0],
	  setofgoal0(Bs, HeadVar, [Name/P|Gy], SG0)	  
	).
setofgoal0([B|Bs], HeadVar, Gy, SetofGoal) :-
	B \= _:_,
	B =.. [Name, _Var], !,
	(
	  memberchk(Name/0, Gy) -> % concept search
	  setofgoal0(Bs, HeadVar, Gy, SetofGoal)
	;
	  (
	    atomic_like_predicate(Name, 1)->
	    SetofGoal = [B|SG0]
	  ;
	    bb_get(aboxmodule, Module),
	    SetofGoal = [Module:B|SG0]
	  ),
	  setofgoal0(Bs, HeadVar, [Name/0|Gy], SG0)
	).
setofgoal0([B|Bs], HeadVar, Gy, SetofGoal) :-
	bb_get(aboxmodule, Module),
	B = Module:_G,
	SetofGoal = [B|SG0],
	setofgoal0(Bs, HeadVar, Gy, SG0).
	
	


position_headvar(G, HeadVar, Res) :-
	G =.. [Name, Var1, Var2], !,
	position([Var1, Var2], HeadVar, P),
	Res = Name/P.
position_headvar(G, _HeadVar, Res) :-
	functor(G, Name, _),
	Res = Name/1.
	
	    
position([Var1, _Var2], HeadVar, P) :-
	(
	  Var1 == HeadVar ->
	  P = 1
	;
	  P = 2
	).

binaries_with_headvar([], _, []).
binaries_with_headvar([G|Gs], HeadVar, Binaries) :-
	G =.. [_,Var1,Var2],
	(Var1 == HeadVar ; Var2 == HeadVar), !,
	Binaries= [G|Bs0],
	binaries_with_headvar(Gs, HeadVar, Bs0).
binaries_with_headvar([G|Gs], HeadVar, Binaries) :-
	G =.. [Name, Var],
	Var == HeadVar,
	query_predicate(Name, 1), !,
	Binaries= [G|Bs0],
	binaries_with_headvar(Gs, HeadVar, Bs0).
binaries_with_headvar([_G|Gs], HeadVar, Binaries) :-
	binaries_with_headvar(Gs, HeadVar, Binaries).

goal_present([C|_Cs], Goal) :-
	goal_present0(C, Goal), !.
goal_present([_|Cs], Goal) :-
	goal_present(Cs, Goal).

goal_present0([G|_Gs], Goal) :-
	G == Goal, !.
goal_present0([_|Gs], Goal) :-
	goal_present0(Gs, Goal).

%%%%%%%%%%%%%%%%%%%%%%%
% statistics predicates
%%%%%%%%%%%%%%%%%%%%%%%

write_statistics :-
	%env_parameter(allinone, yes),
	env_parameter(statistics, yes), !,
	nl,
	headwrite('Auxiliary predicates'),
	init_statistics, nl,
	count, nl,
	get_counts, nl,
	ans_safe_assert,
	stat_meta,
	write('\n:- dlog:init_statistics.\n\n').
write_statistics.


init_statistics :-
	Head = dlog:init_statistics,
	counter_zero(Body0),
	(
	  (env_parameter(projection, no) ; env_parameter(filter_duplicates, yes)) ->
	  Body = (Body0, bb_put(solnum, 0), retractall(dlog:ans(_)))
	;
	  Body = (Body0, bb_put(solnum, 0))
	),
	portray_clause((Head :- Body)).

counter_zero(Body) :-
	findall(C, counter(C), L),
	counter_zero0(L, Body0),
	body_to_list(Body, Body0).

counter_zero0([], []).
counter_zero0([C|Cs], [bb_put(C, 0)|Bs]) :-
	counter_zero0(Cs, Bs).

count :-
	Head = dlog:count(C),
	Body = (bb_get(C, V), NV is V+1, bb_put(C, NV)),
	portray_clause((Head :- Body)).

ans_safe_assert :-
	(env_parameter(projection, no) ; env_parameter(filter_duplicates, yes)), !, 
	Head = dlog:safe_assert(G),
	Body = (\+ dlog:ans(G), bb_get(solnum, S), S1 is S+1, bb_put(solnum, S1), assert(dlog:ans(G))),
	portray_clause((Head :- Body)), nl.
ans_safe_assert.

get_counts :-
	Head = dlog:get_counts(CVs),
	findall(C,counter(C), L),
	count_list(L, Bs, Ps),
	append(Bs, [CVs = Ps], Body0),
	body_to_list(Body, Body0),
	portray_clause((Head :- Body)).

count_list([], [], []).
count_list([C|Cs], [bb_get(C, V)|Bs], [C=V|Ps]) :-
	count_list(Cs, Bs, Ps).

stat_meta :-
	Head = dlog:statistics_call(Goal, HeadVar),
	solution_branch(Goal, SolutionBranch),
	fail_branch(T0, HeadVar, FailBranch),
	Body = (dlog:init_statistics, statistics(runtime, [T0,_]), (SolutionBranch ; FailBranch)),
	portray_clause((Head :- Body)).
	


%%%%%%%%%%%%%%%%%
% auxiliary preds
%%%%%%%%%%%%%%%%%

safe_assert(G) :-
	(
	  call(G)->
	  true
	;
	  assert(G)
	).

unkey([], []).
unkey([_-A|As],[A|Bs]):-
	unkey(As, Bs).

varmemberchk(Element, [Element1|_]) :-
	Element == Element1, !.
varmemberchk(Element, [_|Rest]) :-
	varmemberchk(Element, Rest).

varinbody(Body, Var) :-
	\+ (numbervars(Body, 0, _), var(Var)).

flat_goals([], true).
flat_goals([G], Goal) :-
	!, Goal = G.
flat_goals([G|Gs], (G, Gs0)) :-
	flat_goals(Gs, Gs0).

% env_parameter(Name, Value) :-
% 	functor(Term, Name, 1),
% 	arg(1, Term, Value0),
% 	(
% 	  call(env_user:Term) -> % if there is a user option set for the given name
% 	  Value0 == Value
% 	;
% 	  % default values
% 	  call(env_default:Term) -> 
% 	  Value == Value0
% 	).

env_parameter(Name, Value) :-
	bb_get(uri, URI),
	get_dlog_option(Name, URI, Value).

flat_list(L1, L2) :-
	flat_list(L1, [], L2).

flat_list([], Gy, Gy).
flat_list([L|Ls], Gy, L2) :-
	append(Gy, L, L1),
	flat_list(Ls, L1, L2).

open_ended([], Var, Var).
open_ended([L|Ls], [L|Ls0], EndVar) :-
	open_ended(Ls, Ls0, EndVar).

query_predicate(Name, Arity) :-
	atomic_predicate(Name, Arity), !.
query_predicate(Name, Arity) :-
	once(atomic_like_predicate(Name, Arity)).

