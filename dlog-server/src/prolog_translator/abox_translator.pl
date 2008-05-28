:- module(abox_translator,[abox2prolog/2]).

:- use_module(library(lists)).
:- use_module('../core/config').
:- target(sicstus) -> 
        use_module(library(system), [datime/1])
        ; true.
:- target(swi) -> 
		use_module('../core/core_swi_tools', [datime/1]), %TODO: innen is kivenni a headert
		use_module(library(listing), [portray_clause/1])
        ; true.
:- use_module(transforming_tools, [headwrite/1]).

% Available options:
% indexing(yes) : [yes, no] whether to generate inverses for roles for efficient indexes
% removed: generate_abox(no): [yes, no] whether to generate an ABox Prolog file
% abox_target(assert): [assert, tempfile, allinonefile]
abox2prolog(URI, abox(ABoxStr)) :-
	generate_abox(URI, ABoxStr).

generate_abox(URI, ABoxStr) :-
	(
	  get_dlog_option(indexing, URI, no) ->
	  Indexing = no
	;
	  Indexing = yes
	),
	(
	  %get_dlog_option(generate_abox, URI, yes) ->
	  get_dlog_option(abox_target, URI, assert) ->
	  abox_module_name(URI, Module),
	  ABox = Module
	;
	  ABox = yes
	),
	generate_abox0(URI, ABoxStr, Indexing, ABox).


generate_abox0(URI, ABoxStr, Indexing, ABox) :-
	ABox == yes, !, 
	abox_headers(URI),
	headwrite('Transformed ABox clauses'),
	transformed_abox(ABoxStr, Indexing, ABox).
generate_abox0(_URI, ABoxStr, Indexing, ABox) :-
	transformed_abox(ABoxStr, Indexing, ABox).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transformations: atomic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transformed_abox([], _Indexing, _ABox).
transformed_abox([P-L|Ps], Indexing, ABox) :-
	(
	  L = [_-[*]|_] ->
	  (
		ABox == yes ->
		format('~n:- discontiguous(\'~w\'/1).~n',[P])
	  ; true
	  ),
	  transformed_abox_concept(L, P, ABox)
	;
	  Indexing == yes ->
	  inverses(L, IL),
	  atom_concat('idx_', P, IP),
  	  (
		ABox == yes ->
		format('~n:- discontiguous(\'~w\'/2).~n',[P]),
 	    format(':- discontiguous(\'~w\'/2).~n',[IP])
	  ; true
	  ),
	  transformed_abox_idx_role(L, P, ABox),
	  transformed_abox_idx_role(IL, IP, ABox)
	;
	  (
		ABox == yes ->
  	    format('~n:- discontiguous(\'~w\'/2).~n',[P])
	  ; true
	  ),
	  transformed_abox_noidx_role(L, P, ABox)
	),
	transformed_abox(Ps, Indexing, ABox).

transformed_abox_concept([], _, _).
transformed_abox_concept([Value-_|Vs], P, ABox) :-
	functor(Term, P, 1),
	arg(1, Term, Value),
	portray_abox_clause(ABox, Term),
	transformed_abox_concept(Vs, P, ABox).

transformed_abox_noidx_role([], _, _).
transformed_abox_noidx_role([A-Bs|Xs], P, ABox) :-
	transformed_abox_noidx_role0(Bs, A, P, ABox),
	transformed_abox_noidx_role(Xs, P, ABox).

transformed_abox_noidx_role0([], _, _, _).
transformed_abox_noidx_role0([B|Bs], A, P, ABox) :-
	functor(Term, P, 2),
	arg(1, Term, A),
	arg(2, Term, B),
	portray_abox_clause(ABox, Term),
	transformed_abox_noidx_role0(Bs, A, P, ABox).

transformed_abox_idx_role([], _, _).
transformed_abox_idx_role([X-[Y|Ys]|Xs], P, ABox) :-
	portray_index(Ys, Y, P, X, ABox),
	transformed_abox_idx_role(Xs, P, ABox).

portray_index([], B, Name, A, ABox) :-
	!, Head =.. [Name, A, B],
	portray_abox_clause(ABox, Head).
portray_index([B2|Bs], B, Name, A, ABox) :-
	atom_concat(Name, '_', AName),
	atom_concat(AName, A, IdxName),
	Head =.. [Name, A, X],
	functor(Body, IdxName, 1),
	arg(1, Body, X),
	portray_abox_clause(ABox, (Head :- Body)),
	idx_clauses([B,B2|Bs], IdxName, IdxClauses),
	(
	  ABox == yes ->
	  indented_clauses(IdxClauses, ABox)
	;
	  not_indented_clauses(IdxClauses, ABox)
	).

idx_clauses([], _IdxName, []).
idx_clauses([B|Bs], IdxName, [Fact|Clauses]) :-
	functor(Fact, IdxName, 1),
	arg(1, Fact, B),
	idx_clauses(Bs, IdxName, Clauses).

indented_clauses([], _).
indented_clauses([C|Cs], ABox) :-
	write('                                  '),
	portray_clause(C),
	indented_clauses(Cs, ABox).

not_indented_clauses([], _).
not_indented_clauses([C|Cs], ABox) :-
	portray_abox_clause(ABox, C),
	not_indented_clauses(Cs, ABox).

old_inverses(L, TL) :-
	bagof(B-As,
	      bagof(A, edge_in_graph(L, A, B), As),
	      TL).

edge_in_graph(L, A, B) :-
	member(A-Bs, L),
	member(B, Bs).

inverses(L, IL) :-
	transpose(L, [], T),
	sort(T, Ts),
	group_inverses(Ts, IL).

transpose([], Gy, Gy).
transpose([A-Bs|Xs], Gy, T) :-
	transpose0(Bs, A, Gy, NGy),
	transpose(Xs, NGy, T).

transpose0([], _, Gy, Gy).
transpose0([B|Bs], A, Gy, O) :-
	transpose0(Bs, A, [B-A|Gy], O).
	
group_inverses([A-B|As], O) :-
	group_inverses0(As, A, [B], [], O).

group_inverses0([], N, Gy1, Gy2, [N-Gy1|Gy2]).
group_inverses0([A-B|As], N, Gy1, Gy2, O) :-
	(
	  N == A ->
	  group_inverses0(As, A, [B|Gy1], Gy2, O)
	;
	  group_inverses0(As, A, [B], [N-Gy1|Gy2], O)
	).

portray_abox_clause(yes, C) :-
	!, portray_clause(C).
portray_abox_clause(Module, C) :-
	assert(Module:C).

abox_headers(URI) :-
	headers,
	abox_module_name(URI, MName),
	format(':- module(\'~w\',[]).\n',[MName]).
%	(
%	 target(swi)->
%	 format(':- style_check(-discontiguous).~n',[])
%	;
%	 format(':- set_prolog_flag(discontiguous_warnings, off).~n',[])
%	).

headers:-
	datime(datime(Year, Month, Day, Hour, Min, Sec)),
	write('\% Automatically generated by the DLog system.\n'),
	write('\% Budapest University of Technology and Economic (BUTE), 2007.\n'),
	format('\% User defined options: ~p ~n',[todo]),
	format('\% Timestamp: ~d.~d.~d, ~d:~d:~d sec ~n~n',[Year, Month, Day, Hour, Min, Sec]).