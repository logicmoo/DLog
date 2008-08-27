:- module(abox_translator,[abox2prolog/2]).

:- use_module(library(lists)).
:- use_module('../core/config').
:- target(sicstus) -> 
        use_module(library(system), [datime/1])
        ; true.
:- target(swi) -> 
		use_module('../core/core_swi_tools', [datime/1]), %TODO: innen is kivenni a headert
		use_module(library(listing), [portray_clause/1]),
		use_module(library(odbc), [odbc_connect/3])
        ; true.
:- use_module(transforming_tools, [headwrite/1]).

% Available options:
% indexing(yes) : [yes, no] whether to generate inverses for roles for efficient indexes
% abox_target(assert): [assert, tempfile, allinonefile] whether to generate an ABox Prolog file
abox2prolog(URI, abox(ABoxStr, DBConnections, DBPredicates)) :-
	get_dlog_option(indexing, URI, Indexing),
	abox_module_name(URI, Module),
	(
	  get_dlog_option(abox_target, URI, assert) ->
	  Target = assert
	;
	  Target = file
	),
	generate_abox0(Module, ABoxStr, DBConnections, DBPredicates, Indexing, Target).


generate_abox0(Module, ABoxStr, DBConnections, DBPredicates, Indexing, Target) :-
	(	Target == file
	->	abox_headers(Module),
		headwrite('Transformed ABox clauses')
	;	true
	),
	transformed_DBConnections(DBConnections, Target, Module),
	transformed_abox(ABoxStr, DBPredicates, Module, Indexing, Target).

transformed_DBConnections([], _Target, _Module).
transformed_DBConnections( [Alias-DSN | DBConnections], Target, Module) :-
	(	Target == assert 
	->	odbc_connect(DSN, Connection, [alias(Alias), access_mode(read)]), %null('$null$')
		assert(Module:'$dlog_open_DB_connection$'(Alias))
	;	portray_clause((:- initialization
				odbc_connect(DSN, Connection, [alias(Alias), access_mode(read)]))),
		portray_clause('$dlog_open_DB_connection$'(Alias))
	),
	transformed_DBConnections(DBConnections, Target, Module).

%TODO: close connection at_halt?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transformations: atomic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transformed_abox([], [], _Module, _Indexing, _Target).
transformed_abox([], [P/A-Connection-Query | DBPreds], Module, Indexing, Target) :- %DB-only preds
	(	A == 1
	->	transformed_DBConcept(P, Connection, Query, Target, Module)
	;	(
		    Indexing == yes -> 
		    atom_concat('idx_', P, IP), %TODO: _
		    transformed_DB_idx_role(P, IP, Connection, Query, Target, Module)
		;
			transformed_DB_noidx_role(P, Connection, Query, Target, Module)
		)
	),
	transformed_abox([], DBPreds, Module, Indexing, Target).
transformed_abox([P-L|Ps], DBPreds, Module, Indexing, Target) :-
	(
	  L = [_-[*]|_] -> %concept
	  transformed_abox_concept(L, P, Target, Module),
	  (
	    selectchk(P/1-Connection-Query, DBPreds, DBPreds1) ->
		transformed_DBConcept(P, Connection, Query, Target, Module)
	  ; DBPreds1 = DBPreds
	  )
	;	%role
	  (
	    Indexing == yes -> 
	    inverses(L, IL),
	    atom_concat('idx_', P, IP),
		format_if_file(Target, '~n:- discontiguous(\'~w\'/2).~n', [P]), 
	    format_if_file(Target, ':- discontiguous(\'~w\'/2).~n', [IP]),
	    transformed_abox_idx_role(L, P, Target, Module),
	    transformed_abox_idx_role(IL, IP, Target, Module),
		(
		  selectchk(P/2-Connection-Query, DBPreds, DBPreds1) ->
		  transformed_DB_idx_role(P, IP, Connection, Query, Target, Module)
		  ; DBPreds1 = DBPreds
		)
	  ;
	    transformed_abox_noidx_role(L, P, Target, Module),
		(
		  selectchk(P/2-Connection-Query, DBPreds, DBPreds1) ->
		  transformed_DB_noidx_role(P, Connection, Query, Target, Module)
		  ; DBPreds1 = DBPreds
		)
	  )
	),
	transformed_abox(Ps, DBPreds1, Module, Indexing, Target).

transformed_abox_concept([], _P, _Target, _Module).
transformed_abox_concept([Value-_|Vs], P, Target, Module) :-
	functor(Term, P, 1),
	arg(1, Term, Value), %Term =.. [P, Value]
	portray_abox_clause(Target, Module, Term),
	transformed_abox_concept(Vs, P, Target, Module).

transformed_abox_noidx_role([], _P, _Target, _Module).
transformed_abox_noidx_role([A-Bs|Xs], P, Target, Module) :-
	transformed_abox_noidx_role0(Bs, A, P, Target, Module),
	transformed_abox_noidx_role(Xs, P, Target, Module).

transformed_abox_noidx_role0([], _A, _P, _Target, _Module).
transformed_abox_noidx_role0([B|Bs], A, P, Target, Module) :-
	functor(Term, P, 2),
	arg(1, Term, A),
	arg(2, Term, B),
	portray_abox_clause(Target, Module, Term),
	transformed_abox_noidx_role0(Bs, A, P, Target, Module).

transformed_abox_idx_role([], _P, _Target, _Module).
transformed_abox_idx_role([X-[Y|Ys]|Xs], P, Target, Module) :-
	portray_index(Ys, Y, P, X, Target, Module),
	transformed_abox_idx_role(Xs, P, Target, Module).

portray_index([], B, Name, A, Target, Module) :-
	Head =.. [Name, A, B],
	portray_abox_clause(Target, Module, Head).
portray_index([B2|Bs], B, Name, A, Target, Module) :-
	atom_concat(Name, '_', AName), %TODO: $
	atom_concat(AName, A, IdxName),
	Head =.. [Name, A, X],
	functor(Body, IdxName, 1),
	arg(1, Body, X),
	portray_abox_clause(Target, Module, (Head :- Body)),
	idx_clauses([B,B2|Bs], IdxName, IdxClauses),
	(
	  Target == file ->
	  indented_clauses(IdxClauses)
	;
	  not_indented_clauses(IdxClauses, Module)
	).

idx_clauses([], _IdxName, []).
idx_clauses([B|Bs], IdxName, [Fact|Clauses]) :-
	functor(Fact, IdxName, 1),
	arg(1, Fact, B),
	idx_clauses(Bs, IdxName, Clauses).

indented_clauses([]).
indented_clauses([C|Cs]) :-
	write('                                  '),
	portray_clause(C),
	indented_clauses(Cs).

not_indented_clauses([], _Module).
not_indented_clauses([C|Cs], Module) :-
	assert(Module:C),
	not_indented_clauses(Cs, Module).


transformed_DBConcept(P, Connection, Query, Target, Module) :-
	functor(Head, P, 1),
	arg(1, Head, X),
	portray_abox_clause(Target, Module, 
		(Head :- odbc_query(Connection, Query, row(X), [types([atom])]))).

transformed_DB_idx_role(P, IP, Connection, Query, Target, Module) :-
	Head1 =.. [P, X, Y],
	portray_abox_clause(Target, Module, 
		(Head1 :- odbc_query(Connection, Query, row(X, Y), [types([atom, atom])]))),
	Head2 =.. [IP, X, Y],
	portray_abox_clause(Target, Module, 
		(Head2 :- odbc_query(Connection, Query, row(Y, X), [types([atom, atom])]))).

transformed_DB_noidx_role(P, Connection, Query, Target, Module) :-
	functor(Head, P, 2),
	arg(1, Head, X),
	arg(2, Head, Y),
	portray_abox_clause(Target, Module, 
		(Head :- odbc_query(Connection, Query, row(X, Y), [types([atom, atom])]))).

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

portray_abox_clause(file, _Module, C) :-
	portray_clause(C).
portray_abox_clause(assert, Module, C) :-
	assert(Module:C).

abox_headers(MName) :-
	headers,
	format(':- module(\'~w\',[]).\n',[MName])
	% ,	%TODO
	% portray_clause((
		% :- use_module(library(odbc), [odbc_connect/3, odbc_query/4])
	% ))
	.

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

format_if_file(file, Format, Params) :-
	format(Format, Params).
format_if_file(assert, _Format, _Params).

%selectchk(A, L, R): like select/3 without leaving a choicepoint
selectchk(A, [H|T], R) :-
	(	A = H 
	->	R = T
	;	R = [H|R1],
		selectchk(A, T, R1)
	).

