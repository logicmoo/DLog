:- module(box_immediate,[abox_immediate/1,tbox_immediate/1,assert_tbox/2]).
:- use_module(abox_translator,[assert_abox/2]).

:-dynamic(enable_immediate).

abox_immediate(_Type):-enable_immediate.
tbox_immediate(_Type):-enable_immediate.

assert_tbox(tbox(TransformedTBox, _EqRoles, _QueryPredicates), URI) :-
	abox_module_name(URI, ABox),
	assert_tbox_transformed(TransformedTBox,  ABox),!.

assert_tbox_transformed([],  _ABox) :- !.
assert_tbox_transformed([Predicate|TransformedTBox],  ABox) :-
	write_predicate_immediate(Predicate,  ABox),
	assert_tbox_transformed(TransformedTBox,  ABox).

write_predicate_immediate(concept(Concept-[], Description, Clauses),  ABox) :-
	format( '~n% Asserting Clauses of ~a concept ~q:~n', [Description, Concept]),
	write_clauses_immediate(Clauses,  ABox).
write_predicate_immediate(role(Role-[], Description, Clauses),  ABox) :-
	format( '~n% Asserting Clauses of ~a role ~q:~n', [Description, Role]),
	write_clauses_immediate(Clauses,  ABox).

write_clauses_immediate([],_ABox).
write_clauses_immediate([Clause|Clauses],  ABox) :-
	write_clause1_immediate(Clause,  ABox),
	write_clauses_immediate(Clauses,  ABox).

write_clause1_immediate((HeadName0-HeadVars :- Body0),  ABox) :-
	tbox_translator:predicate_name(HeadName0, HeadName),        
	Head =.. [HeadName|HeadVars],
	tbox_writer:create_body(Body0, Body, ABox),
	assertz( (Head :- Body)).


