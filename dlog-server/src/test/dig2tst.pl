:- module(dig2tst, [dig2tst/1, dig2tst/3]).

:- use_module('../interfaces/dig_reader', [read_dig/2]).


dig2tst(Files) :-
	dig2tst(Files, [], []).
dig2tst(Files, Options, Queries) :-
	(	atom(Files) 
	->	expand_file_name(Files, List)
	;	List = Files
	),
	dig2tst0(List, Options, Queries).

dig2tst0([], _Options, _Queries).
dig2tst0([In | Files], Options, Queries) :-
	catch(
		(read_dig(In, DIG)
		->	DIG = _NS-tells(_URI, Axioms),
			setup_and_call_cleanup(
				(	(	atom_concat(Base, '.dig', In)
					->	atom_concat(Base, '.tst', OutF)
					;	atom_concat(In, '.tst', OutF)
					),
					open(OutF, write, Out)
				),
				write_test(Axioms, Options, Queries, In, Out),
				close(Out)
				),
				format(user_error, 'File completed ~q~n', [In])
			;	format(user_error, 'Bad file: ~q~n', [In])
		),
		E,
		format(user_error, 'Exception while reading file ~q: ~q~n', [In, E])
	),
	dig2tst0(Files, Options, Queries).

write_test(Axioms, Options, Queries, In, Out) :-
	format(Out, '%Generated from ~q.~n~n', [In]),
	write_options(Options, Out),
	write_queries(Queries, Out),
	write_Axioms(Axioms, Out).

write_options([], Out) :- nl(Out).
write_options([H|T], Out) :-
	portray_clause(Out, options(H)),
	write_options(T, Out).

write_queries([], Out) :- nl(Out).
write_queries([Q-D|T], Out) :-
	portray_clause(Out, query(Q, D)),
	write_queries(T, Out).

write_Axioms(axioms(ImpliesCL, ImpliesRL, TransL, ABox, Concepts, Roles, DBConnections, DBPredicates), Out) :-
	write_concepts(Concepts, Out),
	write_roles(Roles, Out),
	write_impliesc(ImpliesCL, Out),
	write_impliesr(ImpliesRL, Out),
	write_trans(TransL, Out),
	write_abox(ABox, Out),
	write_connections(DBConnections, Out),
	write_dbpredicates(DBPredicates, Out).

write_concepts([], Out) :- nl(Out).
write_concepts([H|T], Out) :-
	portray_clause(Out, concept(H)),
	write_concepts(T, Out).

write_roles([], Out) :- nl(Out).
write_roles([H|T], Out) :-
	portray_clause(Out, role(H)),
	write_roles(T, Out).

write_impliesc([], Out) :- nl(Out).
write_impliesc([implies(Concept1, Concept2)|T], Out) :-
	portray_clause(Out, implies(Concept1, Concept2)),
	write_impliesc(T, Out).

write_impliesr([], Out) :- nl(Out).
write_impliesr([subrole(Role1, Role2)|T], Out) :-
	portray_clause(Out, subrole(Role1, Role2)),
	write_impliesr(T, Out).

write_trans([], Out) :- nl(Out).
write_trans([H|T], Out) :-
	portray_clause(Out, transitive(H)),
	write_trans(T, Out).

write_abox([], Out) :- nl(Out).
write_abox([H|T], Out) :-
	(	H = cassertion(C, I) ->
		portray_clause(Out, cassertion(C, I))
	;	H = rassertion(C, I1, I2) ->
		portray_clause(Out, rassertion(C, I1, I2))
	% ;	H = avalue(I, Attribute, Value) ->
		% portray_clause(Out, vassertion(I, Attribute, Value))
	),
	write_abox(T, Out).

write_connections([], Out) :- nl(Out).
write_connections([connection(Name, DSN, User, Password)|T], Out) :-
	portray_clause(Out, dbConnection(Name, DSN, User, Password)),
	write_connections(T, Out).

write_dbpredicates([], Out) :- nl(Out).
write_dbpredicates([access(Functor, Connection, Access)|T], Out) :-
	portray_clause(Out, dbAccess(Functor, Connection, Access)),
	write_dbpredicates(T, Out).








