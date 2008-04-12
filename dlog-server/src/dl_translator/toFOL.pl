:- module(toFOL, [toClause_list/2]).

:- use_module(library(lists)).
:- use_module(bool, [boolneg/2, boolinter/2, boolunion/2]).
:- use_module(show).

toFOL_list(L,R):-
	toFOL_list(L,_,R).

toFOL_list([],_,[]).
toFOL_list([L|Ls],X,[R|Rs]):-
	toFOL(L,X,R),
	toFOL_list(Ls,X,Rs).

toFOL(top,_,top).
toFOL(bottom,_,bottom).
toFOL(aconcept(C),X,aconcept(C,X)):- !.
toFOL(nconcept(C),X,nconcept(C,X)):- !.
toFOL(not(C),X,not(D)):-
	toFOL(C,X,D).
toFOL(and(L),X,and(R)):- !,
	toFOL_list(L,X,R).
toFOL(or(L),X,or(R)):- !,
	toFOL_list(L,X,R).

/*
toFOL(atmost(0,R,C),X,or([not(R2)|Rest])):- !,
	toFOL(R,X,Y,R2),
	boolneg(C,C2),
	toFOL(C2,Y,Rest1),
	(
	  Rest1 = or(Rest), !
	; Rest1 = bottom -> Rest = []
	; Rest = Rest1
	).
*/

toFOL(atmost(N,R,C),X,or(Literals)):-
	boolneg(C,C2),
	N1 is N + 1,
	createVars(N1,Vars),
	atmostLiterals(Vars,R,C2,X,Literals).


toFOL(arole(R),X,Y,arole(R,X,Y)):- !.
toFOL(inv(arole(R)),X,Y,arole(R2,X,Y)):- % TODO ezt majd szepiteni kell
	atom_concat('inv_',R,R2).


% createVars(+N,-Vars):- Vars N darab uj valtozobol allo lista
createVars(0,[]):- !.
createVars(N,[_|Rest]):-
	N > 0,
	N1 is N - 1,	
	createVars(N1,Rest).
	

% atmostLiterals(+Vars,+Role,+Concept,+X,-Literals)
atmostLiterals([],_,_,_,[]).	
atmostLiterals([V|Vars],R,C,X,[not(R2)|Literals]):-
	toFOL(R,X,V,R2),
	toFOL(C,V,FOLC),
	(
	  FOLC = or(FOLC2) -> append(FOLC2,Rest1,Literals)
	; FOLC = bottom -> Literals = Rest1
	; Literals = [FOLC|Rest1]
	),
	equalLiterals(V,Vars,Equals),	
	append(Equals,Rest,Rest1),
	atmostLiterals(Vars,R,C,X,Rest).

equalLiterals(_,[],[]).
equalLiterals(V,[V2|Vars],[eq(V,V2)|Equals]):-
	equalLiterals(V,Vars,Equals).


toClause_list(L,R):-
	toFOL_list(L,L2),

	% nl, show(L2), nl,

	findall(C, (
		     member(A,L2),
		     one_conjunct(A,B),
		     sort(B,C)
		   ), R
	       ).

one_conjunct(and(L),D):- !,
	member(C,L),
	one_conjunct(C,D).
one_conjunct(or([C]),D):- !,
	one_conjunct(C,D).
one_conjunct(or([C|Cs]),X):- !,
	one_conjunct(C,D),
	one_conjunct(or(Cs),Ds),
	append(D,Ds,X).
one_conjunct(X,[X]).



/*********************** Redundans klozok elhagyasa *******************************/

% redundant(+C, +Clauses): igaz, ha van C-nel szukebb kloz a Clauses
% klozhalmazban
% nincs behelyettesites
% a klozoknak meg van adva a tipusuk is
redundant(_,Cs):-
	member([],Cs), !.
redundant([_,C],_):-
	member(true,C), !.
redundant([1,C],Cs):- !,
	member([1,C],Cs).

redundant([TypeC,C],Clauses):-
	member([TypeD,D],Clauses),
	length(D,LD),
	length(C,LC),
	LC >= LD,
	copy_term(D,D2),
	copy_term(C,C2),
	term_variables(C2,Vars),
	includes([TypeC,C2],[TypeD,D2]),
	none_related(Vars), !.

% includes(+C,+D): C kloz tartalmazza D-t
% !!! behelyettesites tortenik !!!		  
includes(_,[_,[]]):- !.
includes([TypeC,C],[TypeD,[LD|D]]):-
	(
	  TypeD = 5
	; TypeC = 10
	; TypeD = 10
	; TypeC = 3, TypeD = 3
	; TypeC = 4, TypeD = 4
	; TypeC = 6, TypeD = 6
	; TypeD = 7-_, TypeD = 7-_
	), !,
	select(LC,C,RestC),
	subsumes(LD,LC),
	includes([TypeC,RestC],[TypeD,D]).
	

% none_related(+L)
% L lista semelyik eleme sincs kapcsolatban egymassal
% tehat nem tartalmazhatja az egyik a masikat
none_related([]).
none_related([A|Rest]):-
	none_related2(A,Rest),
	none_related(Rest).

% none_related2(+A,+L)
% L lista semelyik eleme sincs kapcsolatban A-val
none_related2(_,[]).
none_related2(A,[R|Rs]):-
	not_related(A,R),
	none_related2(A,Rs).

% not_related(+A,+B)
% A es B egyike sem tartalmazza a masikat
not_related(A,B):-
	(	var(A), var(B)	->	\+ A == B
	;	nonvar(A)	->	term_variables(A,As),
	                                distinct_variable(B,As)
	;	nonvar(B)	->	term_variables(B,Bs),
					distinct_variable(A,Bs)
	).

% distinct_variable(+A,+Vars)
% A kulonbozik Vars valtozolista minden elemetol
distinct_variable(_,[]).
distinct_variable(A,[B|Bs]):-
	(
	  A == B -> fail
	; distinct_variable(A,Bs)
	).


% elim_reds(+Clauses,+C,-Reduced): Clauses klozhalmazbol a C miatt redundans
% klozokat elhagyva kapjuk a Reduced klozhalmazt
elim_reds([],_,[]).
elim_reds([A|Rest],C,Reduced):-
	redundant(A,[C]), !,
	elim_reds(Rest,C,Reduced).
elim_reds([A|Rest],C,[A|Reduced]):-
	elim_reds(Rest,C,Reduced).



% remove_redundant(+L,-R),
% L klozlistabol kikuszobolve a redundansakat kapjuk R klozlistat
% a klozok tipussal szerepelnek
remove_redundant(L,R):-
	remove_redundant(L,[],R).


remove_redundant([],R,R).
remove_redundant([L|Ls],Acc,R):-
	redundant(L,Acc), !,
	remove_redundant(Ls,Acc,R).
remove_redundant([L|Ls],Acc,R):-
	elim_reds(Acc,L,Acc2),
	remove_redundant(Ls,[L|Acc2],R).



	
/****************************************************************************************/
/*************** Az ujonnan bevezetett fogalmak eliminalasa *****************************/
/****************************************************************************************/

% remove_temp(+L,-R)
% L klozok listaja, melyeket telitve a bevezetett fogalmak szerint
% es elhagyva ezen fogalmakat tartalmazo klozokat kapjuk R klozlistat
% ha vegtelen ciklusba esnenk, akkor meghagyjuk az adott bevezetett fogalmat
% L-ben a klozok tipusukkal egyutt szerepelnek, mig R-ben tipus nelkul vannak
remove_temp(L,R):-
	contains_struct2(L,nconcept(Pred,_)),
	\+ (
	     member([_,Cls],L),
	     member(nconcept(Pred,_),Cls),
	     member(not(nconcept(Pred,_)),Cls)
	   ),
	!,
	saturate_specific(L,Pred,L2),
	omit_structs(L2,nconcept(Pred,_),L3),	
	remove_temp(L3,R).
remove_temp(L,L).


% saturate_specific(+L,+PredName,-R)
% L klozok listaja, melyeket telitve a PredName nevu bevezetett fogalom szerint
% kapjuk R klozlistat
saturate_specific(L,PredName,R):-
	saturate_specific([],L,PredName,R).
	
	
% saturate_specific(+W1,+W2, +PredName, -R): W1 es W2 klozok listaja, R-t W1 es W2
% telitesevel nyerjuk a PredName nevu bevezetett fogalom szerint
% ugy hogy W1 es W2-beli klozokat, valamint W2-W2-beli klozokat rezolvalunk egymassal
saturate_specific(W1,[],_,W1).
saturate_specific(W1,[C|W2],PredName,R):-
	redundant(C,W1), !, 
	saturate_specific(W1,W2,PredName,R).
saturate_specific(W1,[C|W2],PredName,S):-	
	findall(R,(
		   member(R1,W1),
		   resolve_specific(R1,C,PredName,R)
		  ),Rs),
	elim_reds(W1,C,EW1),
	append(Rs,W2,EW2),
	saturate_specific([C|EW1],EW2,PredName,S).		


% resolve_specific(+Clause1,Clause2,PredName,Resolvant): Resolvant Clause1 es Clause2 kloz PredName uj
% fogalom szerinti rezolvense
resolve_specific([_,C1],[_,C2],PredName,[10,R]):-
	copy_term(C1,Res1),
	copy_term(C2,Res2),
	(	select(nconcept(PredName,X),Res1,M1),		
		select(not(nconcept(PredName,X)),Res2,M2)		
	;	select(not(nconcept(PredName,X)),Res1,M1),		
		select(nconcept(PredName,X),Res2,M2)
	),
	append(M1,M2,L),sort(L,L1),
	simplifyClause([_,L1],[_,R]).
