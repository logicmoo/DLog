:- module(remove_double_proof,[remove_double_proof_list/2]).
:- use_module('../struct').
:- use_module(library(lists)).
% remove_double_proof_list(+L,-S)
% Ha az L beli klozban vannak olyan literalparok, melyek eliminalasa ugyanannak
% a bizonyitasnak az ismetleset jelentene, akkor kiszurjuk az egyiket
% a keletkezo klozlista S
remove_double_proof_list([],[]).
remove_double_proof_list([L|Ls],[R|Rs]):-
	remove_double_proof(L,R),
	remove_double_proof_list(Ls,Rs).

% remove_double_proof(+L,-S)
% Ha az L klozban vannak olyan literalparok, melyek eliminalasa ugyanannak
% a bizonyitasnak az ismetleset jelentene, akkor kiszurjuk az egyiket
% a keletkezo kloz S
remove_double_proof(L,S):-
	select(not(role(R,X,Y1)),L,L1),
	select(not(role(R,X,Y2)),L1,L2),
	

	\+ (
	     contains_struct_substitute(L2,concept(C,_)),
	     (
	       member(M1,L2), M1 == concept(C,Y1), \+ (member(M2,L2), M2 == concept(C,Y2))
	     ; member(M1,L2), M1 == not(concept(C,Y1)), \+ (member(M2,L2), M2 == not(concept(C,Y2)))
	     ; member(M1,L2), M1 == concept(C,Y2), \+ (member(M2,L2), M2 == concept(C,Y1))
	     ; member(M1,L2), M1 == not(concept(C,Y2)), \+ (member(M2,L2), M2 == not(concept(C,Y1)))
	     )
	   ),
	\+ (member(M,L2), (M == role(eq,Y1,Y2); M == role(eq,Y2,Y1))), !,
	Y1 = Y2,
	sort(L,LReduced),
	remove_double_proof(LReduced,S).
remove_double_proof(L,L).