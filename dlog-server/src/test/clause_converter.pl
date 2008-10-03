:- module(clause_converter, [convert_clauses/2, write_converted_clauses/2]).

:- use_module(dlog_test, [read_test_file/4]).
:- use_module(dlog_test, [read_test_file/4]).
:- use_module('../dl_translator/axioms_to_clauses', [axioms_to_clauses/4]).
:- use_module('../core/config', [target/1]).
:- use_module(library(lists), [select/3]).

:- target(sicstus) -> use_module(dlog_test_sicstus_tools, 
		[setup_and_call_cleanup/3]) ; true.

:- target(swi) -> op(1200, xfx, user:(:--))
	; op(1200, xfx, :--).

write_converted_clauses(InFile, OutFile) :-
	read_test_file(InFile, Axioms, _Queries, _Options),
	Axioms = axioms(ImpliesCL, ImpliesRL, TransL, ABox, _Concepts, _Roles, _DBConnections, _DBPredicates),
	axioms_to_clauses([ImpliesCL, ImpliesRL, TransL], _Previous, Clauses, _Save),
	convert_clauses(Clauses, Implications),
	setup_and_call_cleanup(
		open(OutFile, write, Stream),
		write_implications(Implications, ABox, Stream),
		close(Stream)
	).

convert_clauses([], []).
convert_clauses([List| Ls], [Implication|Is]) :-
	select(aconcept(Name, Var), List, Body), !,
	functor(Head, Name, 1),
	arg(1, Head, Var),
	clean(Body, CBody),
	Implication = (Head :-- CBody),
	convert_clauses(Ls, Is).

clean([], true).
clean([Term], CTerm) :-
	clean_term(Term, CTerm).
clean([Term, Term1|Terms], (CTerm, CTerms)) :-
	clean_term(Term, CTerm),
	clean([Term1|Terms], CTerms).

%TODO: nconcept?
clean_term(aconcept(Name, Var), Concept) :-
	functor(Concept, Name, 1),
	arg(1, Concept, Var).
clean_term(arole(Name, Var1, Var2), Role) :-
	functor(Role, Name, 2),
	arg(1, Role, Var1),
	arg(2, Role, Var2).
clean_term(not(Term), \CTerm) :-
	(	Term = aconcept(Name, Var)
	->	functor(CTerm, Name, 1),
		arg(1, CTerm, Var)
	;	Term = arole(Name, Var1, Var2) 
	->	functor(CTerm, Name, 2),
		arg(1, CTerm, Var1),
		arg(2, CTerm, Var2)
	).

write_implications(Implications, ABox, Stream) :-
	portray_clauses(Implications, Stream),
	nl(Stream),
	write_abox(ABox, Stream),
	nl(Stream).

portray_clauses([], _Stream).
portray_clauses([Clause|Clauses], Stream) :-
	portray_clause(Stream, Clause),
	portray_clauses(Clauses, Stream).

write_abox([], _Stream).
write_abox([Assertion |As], Stream) :-
	(	Assertion = cassertion(Concept, Individual)
	->	(	Concept = aconcept(Name)
		->	functor(Fact, Name, 1),
			arg(1, Fact, Individual)
		;	Concept = not(aconcept(Name))
		->	functor(Fact, Name, 1),
			arg(1, Fact, not(Individual)) %TODO
		)
	;	Assertion = rassertion(arole(Name), Individual1, Individual2)
	->	functor(Fact, Name, 2),
		arg(1, Fact, Individual1),
		arg(2, Fact, Individual2)
	),
	portray_clause(Stream, Fact),
	write_abox(As, Stream).

