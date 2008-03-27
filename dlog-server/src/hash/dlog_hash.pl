:- module(anc_loop, [init_state/1,
		     new_state/3,new_anc/3,new_loop/3,
		     check_anc/2,check_loop/2]).

foreign(new_loop, new_loop(+term,+term,+term)).
foreign(check_loop, check_loop(+term,+term,[-integer])).
foreign_resource(dlog_hash, [new_loop,check_loop]).
:- load_foreign_resource(dlog_hash).


:- use_module(library(terms)).
:- use_module(library(lists)).

init_state(LoopHash-AncList) :-
	init_hash(LoopHash),
	AncList = [].

new_state(Goal, LH-AL0, LH-AL) :-
	new_loop(Goal, LH-AL0, _),
	AL = [Goal|AL0].


% new_loop(Goal, LH-AL, LH-AL) :-
% 	add_goal(Goal, LH).

new_anc(Goal, LH-AL0, LH-AL) :-
	AL = [Goal|AL0].

add_goal(Goal, LH) :-
	hash_elem(Goal, LH, Elem),
	memberchk(Goal, Elem).

hash_elem(Goal, LH, Elem) :-	
	term_hash(Goal, -1, 49999, Hash),
	nonvar(Hash),
	H1 is Hash//250+1,
	H2 is Hash mod 250 +1,
	arg(H1, LH, LHRow),
	arg(H2, LHRow, Elem).

check_anc(Goal, _-AL) :-
	member(Goal, AL).

check_loop(Goal, St) :-
	check_loop(Goal, St, 1).
% check_loop(Goal, LH-_) :-
% 	hash_elem(Goal, LH, Elem),
% 	membereq(Elem, Goal).

membereq(List, _) :- var(List), !, fail.
membereq([Y|_], X) :- X == Y.
membereq([_|List], X) :-
	membereq(List, X).

init_hash(Hash) :-
	init_hash(200, 250, HashL),
	Hash =.. [f|HashL].

init_hash(0, _, HL) :-
	!, HL=[].
init_hash(N, M, [H|HL]) :-
	functor(H, f, M),
	N1 is N-1,
	init_hash(N1, M, HL).

:- multifile user:portray/1.

user:portray(Hash) :-
	functor(Hash, f, 200),
	write('hash:\n'),
	(   between(1, 200, Row),
	    between(1, 255, Col),
	    arg(Row, Hash, R),
	    arg(Col, R, C),
	    C \= '$VAR'(_),
	    write(((Row,Col)->C)),
	    nl,
	    fail
	;   true
	).

between(N, _M, N).
between(N, M, X) :-
	N < M,
	N1 is N+1,
	between(N1, M, X).
	
