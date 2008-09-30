% Available options:
% indexing(yes) : [yes, no] whether to generate inverses for roles for efficient indexes
% TODO: + initialization? silentDB, DB used?
:- module(abox_translator,[assert_abox/2, write_abox/2]).

:- use_module(library(lists)).
:- use_module('../core/config').
:- target(sicstus) -> 
        use_module(library(system), [datime/1]),
		use_module(prolog_translator_sicstus_tools, [(thread_local)/1, concat_atom/2])
        ; true.
:- target(swi) -> 
		use_module(prolog_translator_swi_tools, [datime/1]), 
		use_module(library(listing), [portray_clause/1]),
		use_module(library(odbc), [odbc_connect/3, odbc_prepare/5])
        ; true.
:- use_module(transforming_tools, [headwrite/1]).


assert_abox(URI, abox(ABoxStr, DBConnections, DBPredicates)) :-
	get_dlog_option(indexing, URI, Indexing),
	abox_module_name(URI, Module),
	transformed_DBConnections(DBConnections, assert, Module),
	%dynamic(Module:'$dlog_active_statement'/2), %Sicstus compatibility
	assert(Module:'$dlog_active_statement'(_,_)),
	retractall(Module:'$dlog_active_statement'(_,_)),
	transformed_abox(ABoxStr, DBPredicates, Module, Indexing, assert).
	%TODO: finalize dynamic? (SWI)

write_abox(URI, abox(ABoxStr, DBConnections, DBPredicates)) :-
	get_dlog_option(indexing, URI, Indexing),
	abox_module_name(URI, Module),
	abox_headers(Module, Indexing),
	headwrite('Transformed ABox clauses'),
	transformed_DBConnections(DBConnections, write, Module),
	retractall('$dlog_active_statement'(_)), %better safe than sorry
	transformed_abox(ABoxStr, DBPredicates, Module, Indexing, write),
	retractall('$dlog_active_statement'(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transformations: atomic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transformed_abox([], [], _Module, _Indexing, _Target) :- !.
transformed_abox([], [access(P/A, Connection, Access) | DBPreds], Module, Indexing, Target) :- %DB-only preds
	selectall(DBPreds, access(P/A, _, _), Accesses, DBPreds1),
	(	A == 1
	->	transformed_DBConcepts([access(P/A, Connection, Access) | Accesses], Target, Module)
	;	(
		    Indexing == yes -> 
		    atom_concat('idx_', P, IP), %TODO: _
			format_if_write(Target, '~n~n:- discontiguous(\'~w\'/2).~n', [P]), 
			format_if_write(Target, ':- discontiguous(\'~w\'/2).~n~n', [IP]),
		    transformed_DB_idx_roles([access(P/A, Connection, Access) | Accesses], Target, Module)
		;
			transformed_DB_noidx_roles([access(P/A, Connection, Access) | Accesses], Target, Module)
		)
	),
	format_if_write(Target, '~n', []), 
	transformed_abox([], DBPreds1, Module, Indexing, Target).
transformed_abox([P-L|Ps], DBPreds, Module, Indexing, Target) :-
	(
	  L = [_-[*]|_] -> %concept
	  transformed_abox_concept(L, P, Target, Module),
	  selectall(DBPreds, access(P/1, _, _), Accesses, DBPreds1),
	  transformed_DBConcepts(Accesses, Target, Module)
	;	%role
	  (
	    Indexing == yes -> 
	    inverses(L, IL),
	    atom_concat('idx_', P, IP), %TODO: _
		format_if_write(Target, '~n~n:- discontiguous(\'~w\'/2).~n', [P]), 
	    format_if_write(Target, ':- discontiguous(\'~w\'/2).~n~n', [IP]),
	    transformed_abox_idx_role(L, P, Target, Module),
	    transformed_abox_idx_role(IL, IP, Target, Module),
		selectall(DBPreds, access(P/2, _, _), Accesses, DBPreds1),
		transformed_DB_idx_roles(Accesses, Target, Module)
	  ;
	    transformed_abox_noidx_role(L, P, Target, Module),
		selectall(DBPreds, access(P/2, _, _), Accesses, DBPreds1),
		transformed_DB_noidx_roles(Accesses, Target, Module)
	  )
	),
	format_if_write(Target, '~n', []), 
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
	  Target == write ->
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Database                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transformed_DBConnections([], _Target, _Module).
transformed_DBConnections( [connection(Alias, DSN, User, Password) | DBConnections], Target, Module) :-
	Options0 = [alias(Alias), 
				access_mode(read)
				%,null('$null$')
				%,silent(true) %TODO: option?
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


transformed_DBConcepts([], _Target, _Module).
transformed_DBConcepts([Access|Accesses], Target, Module) :-
	transformed_DBConcept(Target, Access, Module),
	transformed_DBConcepts(Accesses, Target, Module).

transformed_DBConcept(assert, access(P/1, Connection, Access), Module) :-
	functor(Head, P, 1),
	arg(1, Head, X),
	(	Access = table(Table, Col) 
	->	prepared_query(['SELECT `', Col, '` FROM `', Table, '`'], 
				Module, Connection, [], [atom], RetrSt),
		prepared_query(['SELECT 1 FROM `', Table, '` WHERE `', Col, '` = ?'], 
				Module, Connection, [default], [atom], CheckSt),
		assert((Module:Head :- 
				var(X) -> 
					odbc_execute(RetrSt, [], row(X))
				;	odbc_execute(CheckSt, [X], _)
			))
	;	Access = query(RetrQ),
		prepared_query([RetrQ], Module, Connection, [], [atom], RetrSt),
		assert((Module:Head :- odbc_execute(RetrSt, [], row(X)))) 
	).
transformed_DBConcept(write, access(P/1, Connection, Access), _Module) :-
	functor(Head, P, 1),
	arg(1, Head, X),
	(	Access = table(Table, Col) 
	->	prepared_query(['SELECT `', Col, '` FROM `', Table, '`'], 
			Connection, [], [atom], RetrQ),
		prepared_query(['SELECT 1 FROM `', Table, '` WHERE `', Col, '` = ?'], 
			Connection, [default], [atom], CheckQ),
		portray_clause((
			Head :- 
				var(X) -> 
					'$dlog_active_statement'(RetrQ, Statement),
					odbc_execute(Statement, [], row(X))
				;	'$dlog_active_statement'(CheckQ, Statement),
					odbc_execute(Statement, [X], _)
			))
	;	Access = query(RetrQ),
		prepared_query([RetrQ], Connection, [], [atom], RetrQ),
		portray_clause((
			Head :- 
				'$dlog_active_statement'(RetrQ, Statement),
				odbc_execute(Statement, [], row(X))
		))
	).


transformed_DB_idx_roles([], _Target, _Module).
transformed_DB_idx_roles([access(P/2, Connection, Access) |Accesses], Target, Module) :-
	atom_concat('idx_', P, IP), %TODO: _
	Head1 =.. [P, X, Y],
	transformed_DB_role(Target, Head1, X, Y, Connection, Access, Module),
	Head2 =.. [IP, Y, X],
	transformed_DB_role(Target, Head2, X, Y, Connection, Access, Module),
	transformed_DB_idx_roles(Accesses, Target, Module).

transformed_DB_noidx_roles([], _Target, _Module).
transformed_DB_noidx_roles([access(P/2, Connection, Access)|Accesses], Target, Module) :-
	functor(Head, P, 2),
	arg(1, Head, X),
	arg(2, Head, Y),
	transformed_DB_role(Target, Head, X, Y, Connection, Access, Module),
	transformed_DB_noidx_roles(Accesses, Target, Module).
	
transformed_DB_role(assert, Head, X, Y, Connection, Access, Module) :-
	(	Access = table(Table, Col1-Col2) 
	->	prepared_query(['SELECT `', Col1, '`, `', Col2 , '` FROM `', Table, '`'], 
				Module, Connection, [], [atom, atom], RetrSt),
		prepared_query(['SELECT `', Col1, '` FROM `', Table, '` WHERE `', Col2, '` = ?'], 
				Module, Connection, [default], [atom], Retr1St),
		prepared_query(['SELECT `', Col2, '` FROM `', Table, '` WHERE `', Col1, '` = ?'], 
				Module, Connection, [default], [atom], Retr2St),
		prepared_query(['SELECT 1 FROM `', Table, '` WHERE `', Col1, '` = ? AND `', Col2, '` = ?'], 
				Module, Connection, [default, default], [atom], CheckSt),
		assert(
			(Module:Head :- 
				var(X) -> 
				(	var(Y) ->
					odbc_execute(RetrSt, [], row(X, Y))
				;	odbc_execute(Retr1St, [Y], row(X))
				)
				;
				(	var(Y) ->
					odbc_execute(Retr2St, [X], row(Y))
				;	odbc_execute(CheckSt, [X, Y], _)
				)
			))
	;	Access = query(RetrQ),
		prepared_query([RetrQ], Module, Connection, [], [atom, atom], RetrSt),
		assert((Module:Head :- odbc_execute(RetrSt, [], row(X, Y))))
	).
transformed_DB_role(write, Head, X, Y, Connection, Access, _Module) :-
	(	Access = table(Table, Col1-Col2) 
	->	prepared_query(['SELECT `', Col1, '`, `', Col2 , '` FROM `', Table, '`'], 
				Connection, [], [atom, atom], RetrQ),
		prepared_query(['SELECT `', Col1, '` FROM `', Table, '` WHERE `', Col2, '` = ?'], 
				Connection, [default], [atom], Retr1Q),
		prepared_query(['SELECT `', Col2, '` FROM `', Table, '` WHERE `', Col1, '` = ?'], 
				Connection, [default], [atom], Retr2Q),
		prepared_query(['SELECT 1 FROM `', Table, '` WHERE `', Col1, '` = ? AND `', Col2, '` = ?'], 
				Connection, [default, default], [atom], CheckQ),
		portray_clause(
			(Head :- 
				var(X) -> 
				(	var(Y) ->
					'$dlog_active_statement'(RetrQ, Statement),
					odbc_execute(Statement, [], row(X, Y))
				;	'$dlog_active_statement'(Retr1Q, Statement),
					odbc_execute(Statement, [Y], row(X))
				)
				;
				(	var(Y) ->
					'$dlog_active_statement'(Retr2Q, Statement),
					odbc_execute(Statement, [X], row(Y))
				;	'$dlog_active_statement'(CheckQ, Statement),
					odbc_execute(Statement, [X, Y], _)
				)
			))
	;	Access = query(RetrQ),
		prepared_query([RetrQ], Connection, [], [atom, atom], RetrQ),
		portray_clause(
			(Head :- 
				'$dlog_active_statement'(RetrQ, Statement),
				odbc_execute(Statement, [], row(X, Y))
			))
	).


%assert
prepared_query(Query, Module, Connection, Conv, Types, Statement) :- 
	concat_atom(Query, PQuery), 
	(	Module:'$dlog_active_statement'(PQuery, Statement) %query already prepared
	->	true 
	;	odbc_prepare(Connection, PQuery, Conv, Statement, [types(Types)]), 
		assert(Module:'$dlog_active_statement'(PQuery, Statement))
	).

:- dynamic '$dlog_active_statement'/1.
:- thread_local '$dlog_active_statement'/1.
%write
prepared_query(Query, Connection, Conv, Types, PQuery) :- 
	concat_atom(Query, PQuery), 
	(	'$dlog_active_statement'(PQuery) %query already prepared
	->	true 
	;	portray_clause((
		 :- odbc_prepare(Connection, PQuery, Conv, Statement, [types(Types)]), 
			assert('$dlog_active_statement'(PQuery, Statement))
		)),
		assert('$dlog_active_statement'(PQuery))
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         /Database                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

portray_abox_clause(write, _Module, C) :-
	portray_clause(C).
portray_abox_clause(assert, Module, C) :-
	assert(Module:C).

abox_headers(MName, Indexing) :-
	headers(Indexing),
	format(':- module(\'~w\',[]).\n',[MName]),
	portray_clause((
		:- current_predicate(config:target/1)
			->
			(config:target(swi) -> use_module(library(odbc)) ; true)
			;
			(current_prolog_flag(dialect, swi) -> use_module(library(odbc))
			; %current_prolog_flag(language, sicstus) %sicstus/iso
			true)
		)),
	nl,
	portray_clause((
		:- dynamic '$dlog_active_statement'/2
	)).


headers(Indexing) :-
	datime(datime(Year, Month, Day, Hour, Min, Sec)),
	write('\% Automatically generated by the DLog system.\n'),
	write('\% Budapest University of Technology and Economic (BME), 2007-2008.\n'),
	format('\% User defined options: ~p ~n',[indexing(Indexing)]), %TODO: any other options?
	format('\% Timestamp: ~d.~d.~d, ~d:~d:~d sec ~n~n',[Year, Month, Day, Hour, Min, Sec]).

format_if_write(write, Format, Params) :-
	format(Format, Params).
format_if_write(assert, _Format, _Params).


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

