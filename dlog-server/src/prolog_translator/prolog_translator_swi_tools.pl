:- module(prolog_translator_swi_tools, [term_variables_bag/2, bb_put/2, bb_get/2, reduce/2, datime/1]).

:- use_module(library(assoc), [get_assoc/3, get_assoc/5, ord_list_to_assoc/2]).


term_variables_bag(X,Y) :- term_variables(X,Y).
	
bb_put(X,Y) :- nb_setval(X,Y).
bb_get(X,Y) :- catch(nb_getval(X,Y), _E, fail).


datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
	get_time(TimeStamp),
	stamp_date_time(TimeStamp, date(Year,Month,Day,Hour,Min,Sec1,_Off,_TZ,_DST), local),
	Sec is floor(Sec1).

%TODO!
%   reduce(+Graph, -Reduced)
%   is true if Reduced is the reduced graph for Graph. The vertices of
%   the reduced graph are the strongly connected components of Graph.
%   There is an edge in Reduced from u to v iff there is an edge in
%   Graph from one of the vertices in u to one of the vertices in v. A
%   strongly connected component is a maximal set of vertices where
%   each vertex has a path to every other vertex.
%   Algorithm from "Algorithms" by Sedgewick, page 482, Tarjan's algorithm.
%   Approximately linear in the maximum of arcs and nodes (O(N log N)).

reduce(Graph, Reduced) :-
   strong_components(Graph, SCCS, Map),
   reduced_vertices_edges(Graph, Vertices, Map, Edges, []),
   sort(Vertices, Vertices1),
   sort(Edges, Edges1),
   group_edges(Vertices1, Edges1, Reduced),
   sort(SCCS, Vertices1).

strong_components(Graph, SCCS, A) :-
   nodeinfo(Graph, Nodeinfo, Vertices), 
   ord_list_to_assoc(Nodeinfo, A0), 
   visit(Vertices, 0, _, A0, A, 0, _, [], _, SCCS, []).

visit([], Min, Min, A, A, I, I, Stk, Stk) --> [].
visit([V|Vs], Min0, Min, A0, A, I, M, Stk0, Stk) -->
   {get_assoc(V, A0, node(Ns,J,Eq), A1, node(Ns,K,Eq))},
   (   {J>0} ->
       {J=K, J=Min1, A1=A3, I=L, Stk0=Stk2}
   ;   {K is I+1},
       visit(Ns, K, Min1, A1, A2, K, L, [V|Stk0], Stk1),
       (   {K>Min1} -> {A2=A3, Stk1=Stk2}
       ;   pop(V, Eq, A2, A3, Stk1, Stk2, [])
       )
   ),
   {Min2 is min(Min0,Min1)},
   visit(Vs, Min2, Min, A3, A, L, M, Stk2, Stk).

pop(V, Eq, A0, A, [V1|Stk0], Stk, SCC0) -->
   {get_assoc(V1, A0, node(Ns,_,Eq), A1, node(Ns,16'100000,Eq))},
   (   {V==V1} -> [SCC], {A1=A, Stk0=Stk, sort([V1|SCC0], SCC)}
   ;   pop(V, Eq, A1, A, Stk0, Stk, [V1|SCC0])
   ).

nodeinfo([], [], []).
nodeinfo([V-Ns|G], [V-node(Ns,0,_)|Nodeinfo], [V|Vs]) :-
   nodeinfo(G, Nodeinfo, Vs).

reduced_vertices_edges([], [], _) --> [].
reduced_vertices_edges([V-Neibs|Graph], [V1|Vs], Map) -->
   {get_assoc(V, Map, N), N=node(_,_,V1)},
   reduced_edges(Neibs, V1, Map),
   reduced_vertices_edges(Graph, Vs, Map).

reduced_edges([], _, _) --> [].
reduced_edges([V|Vs], V1, Map) -->
   {get_assoc(V, Map, N), N=node(_,_,V2)},
   ({V1==V2} -> []; [V1-V2]),
   reduced_edges(Vs, V1, Map).
   
group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
   group_edges(Edges, Vertex, Neibs, RestEdges),
   group_edges(Vertices, RestEdges, G).

group_edges([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
   group_edges(Edges, V, Neibs, RestEdges).
group_edges(Edges, _, [], Edges).