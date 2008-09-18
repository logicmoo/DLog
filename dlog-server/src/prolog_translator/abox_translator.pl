:- module(abox_translator,[abox2prolog/2]).

:- use_module(library(lists)).
:- use_module('../core/config').
:- target(sicstus) -> 
        use_module(library(system), [datime/1]),
		use_module(core_sicstus_tools, [concat_atom/2])
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
transformed_DBConnections( [connection(Alias, DSN, User, Password) | DBConnections], Target, Module) :-
	Options0 = [alias(Alias), 
				access_mode(read)
				%,null('$null$')
				%,silent(true)
				], 
	(	nonvar(User) 
	->	Options1 = [user(User)| Options0]
	;	Options1 = Options0
	),
	(	nonvar(Password) 
	->	Options = [password(Password)| Options1]
	;	Options = Options1
	),
	(	Target == assert 
	->	odbc_connect(DSN, Connection, Options), 
		assert(Module:'$dlog_open_DB_connection'(Alias))
	;	portray_clause((:- %TODO: initialization? -> close connection at_halt?
				odbc_connect(DSN, Connection, Options))),
		portray_clause('$dlog_open_DB_connection'(Alias))
	),
	transformed_DBConnections(DBConnections, Target, Module).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transformations: atomic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transformed_abox([], [], _Module, _Indexing, _Target) :- !.
transformed_abox([], [access(P/A, Connection, Access) | DBPreds], Module, Indexing, Target) :- %DB-only preds
	selectall(DBPreds, access(P/A, _, _), Accesses, DBPreds1),
	(	A == 1
	->	transformed_DBConcept([access(P/A, Connection, Access) | Accesses], Target, Module)
	;	(
		    Indexing == yes -> 
		    atom_concat('idx_', P, IP), %TODO: _
			format_if_file(Target, '~n~n:- discontiguous(\'~w\'/2).~n', [P]), 
			format_if_file(Target, ':- discontiguous(\'~w\'/2).~n~n', [IP]),
		    transformed_DB_idx_role([access(P/A, Connection, Access) | Accesses], Target, Module)
		;
			transformed_DB_noidx_role([access(P/A, Connection, Access) | Accesses], Target, Module)
		)
	),
	transformed_abox([], DBPreds1, Module, Indexing, Target).
transformed_abox([P-L|Ps], DBPreds, Module, Indexing, Target) :-
	(
	  L = [_-[*]|_] -> %concept
	  transformed_abox_concept(L, P, Target, Module),
	  selectall(DBPreds, access(P/1, _, _), Accesses, DBPreds1),
	  transformed_DBConcept(Accesses, Target, Module)
	;	%role
	  (
	    Indexing == yes -> 
	    inverses(L, IL),
	    atom_concat('idx_', P, IP), %TODO: _
		format_if_file(Target, '~n~n:- discontiguous(\'~w\'/2).~n', [P]), 
	    format_if_file(Target, ':- discontiguous(\'~w\'/2).~n~n', [IP]),
	    transformed_abox_idx_role(L, P, Target, Module),
	    transformed_abox_idx_role(IL, IP, Target, Module),
		selectall(DBPreds, access(P/2, _, _), Accesses, DBPreds1),
		transformed_DB_idx_role(Accesses, Target, Module)
	  ;
	    transformed_abox_noidx_role(L, P, Target, Module),
		selectall(DBPreds, access(P/2, _, _), Accesses, DBPreds1),
		transformed_DB_noidx_role(Accesses, Target, Module)
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
	atom_concat(Name, '_', AName), %TODO: _
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


transformed_DBConcept([], _Target, _Module).
transformed_DBConcept([access(P/1, Connection, Access)|Accesses], Target, Module) :-
	functor(Head, P, 1),
	arg(1, Head, X),
	(	Access = table(Table, Col) 
	->	concat_atom(['SELECT `', Col, '` FROM `', Table, '`'], RetrQ), 
		concat_atom(['SELECT 1 FROM `', Table, '` WHERE `', Col, '` = ?'], CheckQ),
		portray_abox_clause(Target, Module, 
			(Head :- 
				var(X) -> odbc_query(Connection, RetrQ, row(X), [types([atom])]) %TODO: prepared statement
				; setup_and_call_cleanup(
						odbc_prepare(Connection, CheckQ, [default], Statement, [types([atom])]), %TODO: reuse prepared statement
						odbc_execute(Statement, [X], _),
						odbc_free_statement(Statement)
					)
			))
	;	Access = query(Query),
		portray_abox_clause(Target, Module, 
			(Head :- odbc_query(Connection, Query, row(X), [types([atom])]))) %TODO: prepared statement?
	),
	transformed_DBConcept(Accesses, Target, Module).

transformed_DB_idx_role([], _Target, _Module).
transformed_DB_idx_role([access(P/2, Connection, Access) |Accesses], Target, Module) :-
	atom_concat('idx_', P, IP), %TODO: _
	Head1 =.. [P, X, Y],
	transformed_DB_role(Head1, X, Y, Connection, Access, Target, Module),
	Head2 =.. [IP, Y, X],
	transformed_DB_role(Head2, X, Y, Connection, Access, Target, Module),
	transformed_DB_idx_role(Accesses, Target, Module).

transformed_DB_noidx_role([], _Target, _Module).
transformed_DB_noidx_role([access(P/2, Connection, Access)|Accesses], Target, Module) :-
	functor(Head, P, 2),
	arg(1, Head, X),
	arg(2, Head, Y),
	transformed_DB_role(Head, X, Y, Connection, Access, Target, Module),
	transformed_DB_noidx_role(Accesses, Target, Module).
	
transformed_DB_role(Head, X, Y, Connection, Access, Target, Module) :-
	(	Access = table(Table, Col1-Col2) 
	->	concat_atom(['SELECT `', Col1, '`, `', Col2 , '` FROM `', Table, '`'], RetrQ), 
		concat_atom(['SELECT `', Col1, '` FROM `', Table, '` WHERE `', Col2, '` = ?'], Retr1Q),
		concat_atom(['SELECT `', Col2, '` FROM `', Table, '` WHERE `', Col1, '` = ?'], Retr2Q),
		concat_atom(['SELECT 1 FROM `', Table, '` WHERE `', Col1, '` = ? AND `', Col2, '` = ?'], CheckQ),
		portray_abox_clause(Target, Module, 
			(Head :- 
				var(X) -> 
				(	var(Y) ->
					odbc_query(Connection, RetrQ, row(X, Y), [types([atom, atom])]) %TODO: prepared statement
				;	setup_and_call_cleanup(
						odbc_prepare(Connection, Retr1Q, [default], Statement, [types([atom])]), %TODO: reuse prepared statement
						odbc_execute(Statement, [Y], row(X)),
						odbc_free_statement(Statement)
					)
				)
				;
				(	var(Y) ->
					setup_and_call_cleanup(
						odbc_prepare(Connection, Retr2Q, [default], Statement, [types([atom])]), %TODO: reuse prepared statement
						odbc_execute(Statement, [X], row(Y)),
						odbc_free_statement(Statement)
					)
				;	setup_and_call_cleanup(
						odbc_prepare(Connection, CheckQ, [default, default], Statement), %TODO: reuse prepared statement
						odbc_execute(Statement, [X, Y], _),
						odbc_free_statement(Statement)
					)
				)
			))
	;	Access = query(Query),
		portray_abox_clause(Target, Module, 
			(Head :- odbc_query(Connection, Query, row(X, Y), [types([atom, atom])]))) %TODO: prepared statement?
	).

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
		% :- use_module(library(odbc))
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


%selectall(L, Pat, Res, Rem): Res is the list of all elements of L that 
%  can be unified with Pat. Rem are the remaining elements.  
selectall([], _Pat, [], []).
selectall([A|L], Pat, Res, Rem) :-
	(	\+ A \= Pat 
	->	Res = [A|Res1],
		Rem = Rem1
	;	Res = Res1,
		Rem = [A|Rem1]
	),
	selectall(L, Pat, Res1, Rem1).

%%selectchk(A, L, R): like select/3 without leaving a choicepoint
% selectchk(A, [H|T], R) :-
	% (	A = H 
	% ->	R = T
	% ;	R = [H|R1],
		% selectchk(A, T, R1)
	% ).

