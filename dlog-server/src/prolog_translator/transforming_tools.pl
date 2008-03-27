:- module(transforming_tools,[headwrite/1, neg/2, add_key/2, unzip_second/2, list_to_open_list/2, contra/2]).

:- use_module(library(lists)).

headwrite(Atom) :-
	atom_codes(Atom, Cs),
	length(Cs, Length),
	write_n(Length, '*'),
	format('\% ~p~n',[Atom]),
	write_n(Length, '*'),
	nl.

write_n(L, C) :-
	write('\% '),
	write_n0(L, C),
	nl.

write_n0(0, _) :- !.
write_n0(N, C) :-
	N > 0,
	N1 is N-1,
	write(C),
	write_n0(N1, C).

neg(Term, NegTerm) :-
	Term =.. [Name|Args],
	negname(Name, NegName),
	NegTerm =.. [NegName|Args].

negname(Name, NegName) :-
	atom_concat('not_', X, Name), !,
	NegName = X.
negname(Name, NegName) :-
	atom_concat('not_', Name, NegName).

contra(LitList0, Contras) :-
	add_key(LitList0, ALitList0),
	keysort(ALitList0, ALitList),
	unzip_second(ALitList, LitList),
	contrapositives(LitList,Contras).

add_key([], []).
add_key([L|Ls], [K-L|KLs]) :-
	functor(L, _, N),
	K is -N,
	add_key(Ls, KLs).

unzip_second([], []).
unzip_second([_-Y|XYs], [Y|Ys]) :-
	unzip_second(XYs, Ys).

contrapositives(L,Contras):-
	findall(C, (
		     contrapositive_of(L,C)
		   ), Contras
	       ).

contrapositive_of(L, (Head:-Body)) :-
	select(NH, L, BL),
	\+ ponalt_binary(NH),
	neg(NH, Head),
	list_to_open_list(BL, Body).

ponalt_binary(T) :-
	functor(T, N, 2),
	\+ atom_concat('not_', _X, N).

list_to_open_list([], true).
list_to_open_list([G],X):- !, X=G.
list_to_open_list([G0|L], G) :-
	(   L = [] -> G = G0
	;   G = (G0,G1),
	    list_to_open_list(L, G1)
	).