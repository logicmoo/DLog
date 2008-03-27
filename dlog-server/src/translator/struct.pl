:- module(struct,[contains_struct/2,contains_struct2/2, all_containing/4, omit_structs/3]).

:- use_module('../config').
:- target(sicstus) 
	-> use_module(library(terms),[subsumes/2,subsumes_chk/2])
	; true.

% contains_struct(+X,+S): X tartalmaz S alaku strukturat
%	nincs behelyettesites
contains_struct(X,S):-
	subsumes_chk(S,X),!.
contains_struct(X,S):-
	nonvar(X),X=..[_|Args],any_contains_struct(Args,S).

% any_contains_struct(+Xs,+S): Xs lista valamelyik eleme tartalmaz S alaku sturkturat
any_contains_struct([X|_],S):-
	contains_struct(X,S),!.
any_contains_struct([_|Xs],S):-
	any_contains_struct(Xs,S).
	
	
	
% contains_struct2(+X,+S): X tartalmaz S alaku strukturat
%	van behelyettesites
contains_struct2(X,S):-
	subsumes(S,X).
contains_struct2(X,S):-
	nonvar(X),X=..[_|Args],any_contains_struct2(Args,S).

% any_contains_struct2(+Xs,+S): Xs lista valamelyik eleme tartalmaz S alaku sturkturat
any_contains_struct2([X|_],S):-
	contains_struct2(X,S).
any_contains_struct2([_|Xs],S):-
	any_contains_struct2(Xs,S).
	
% all_containing(+Clause,+Pattern,-Contains,-Rest)
% Clause lista azon elemei, melyek tartalmaznak Pattern-nel
% egyesitheto reszt kerulnek Contains-ba, a maradek pedig
% Rest-be
all_containing([],_,[],[]).
all_containing([L|Clause],Pattern,[L|Contains],Rest):-
	contains_struct(L,Pattern),
	!, all_containing(Clause,Pattern,Contains,Rest).
all_containing([L|Clause],Pattern,Contains,[L|Rest]):-
	all_containing(Clause,Pattern,Contains,Rest).
	
	
% omit_structs(+Cs,+S,-Ofs): Ofs listat ugy kapjuk, hogy a Cs klozlistabol
% elhagyjuk az S alaku strukturat tartalmazokat
omit_structs([C|Cs],S,Os):-
	contains_struct(C,S),!,omit_structs(Cs,S,Os).
omit_structs([C|Cs],S,[C|Os]):-
	omit_structs(Cs,S,Os).
omit_structs([],_,[]).