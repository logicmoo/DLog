:- module(prolog_translator_sicstus_tools, [thread_local/1, concat_atom/2]).

:- op(1150, fx, thread_local).

thread_local(_).

%concat_atom(+List, -Atom)
concat_atom([Atom0|List], Atom) :-
	concat_atom0(List, Atom0, Atom).

concat_atom0([], Atom, Atom).
concat_atom0([Atom1|List], Atom0, Atom) :-
	atom_concat(Atom0, Atom1, Atom2),
	concat_atom0(List, Atom2, Atom).

