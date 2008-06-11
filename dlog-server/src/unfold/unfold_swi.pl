term_variables_prefix(X, Vars) :-
	term_variables(X, XVars, _XVarsTail),
	XVars = Vars.

bb_put(Var, Val) :-
	nb_setval(Var, Val).

bb_get(Var, Val) :-
	nb_getval(Var, Val).

ord_member(Elem, Set) :-
	ord_memberchk(Elem, Set).

transpose_ugraph(G, TG) :-
	transpose(G, TG).

%   ord_intersection(+Sets, ?Intersection)
%   is true when Intersection is the ordered set representation of the
%   intersection of all the sets in Sets.

ord_intersection(Sets, Intersection) :- 
	length(Sets, NumberOfSets),
	NumberOfSets > 0,
	ord_intersection2(NumberOfSets, Sets, Intersection, []).

ord_intersection2(1, [Set|Sets], Set0, Sets0) :- !,
	Set = Set0,
	Sets = Sets0.
ord_intersection2(2, [Set,Set2|Sets], Intersection, Sets0) :- !,
	Sets = Sets0,
	ord_intersection(Set, Set2, Intersection).
ord_intersection2(N, Sets0, Intersection, Sets) :-
%	N > 2,
	A is N>>1,
	Z is N-A,
	ord_intersection2(A, Sets0, X, Sets1),
	ord_intersection2(Z, Sets1, Y, Sets),
	ord_intersection(X, Y, Intersection).


% needed only for unfold_test.pl
%target(swi).

current_directory_files(Fs) :-
	expand_file_name(*, Fs).

file_mod_time(File, MT) :-
	time_file(File, MT).

:- ensure_loaded(unfold_test).
