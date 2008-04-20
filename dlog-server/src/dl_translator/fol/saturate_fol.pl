:- module(saturate_fol,[saturate/2, saturate_partially/3, resolve/3, redundant/2, remove_redundant/2, elim_reds/3, simplifyClauses/2, simplifyClause/2,  remove_temp/2]).

:- use_module('../../config').
:- use_module('../show').
:- use_module('../struct').
:- use_module('selectResolvable_fol').
:- use_module(library(lists),[append/3,member/2,select/3,delete/3]).
:- target(sicstus) -> 
        use_module(library(terms),[subsumes/2, term_variables/2])
        ; true.
:- use_module(library(ordsets),[list_to_ord_set/2,ord_subset/2]).

/***************************** Klozhalmaz telitese **********************************/

% saturate(+W,-S):-
% W klozok listaja, melyek telitesevel kapjuk S-et
saturate(W,S):-
	simplifyClauses(W,W2),
	selectResolvable(W2,Selected),

	% 7-es tipusu klozok a vegere
	seven_to_end(Selected,Ordered),

	saturate([],Ordered,S).
% saturate(+W1, +W2, -S):- W1 rendezett es telitett klozok listaja
% W1 es W2 egyuttes telitesevel kapjuk S klozlistat
saturate_partially(W1,W2,S):-
	simplifyClauses(W2,W21),
	selectResolvable(W21,W22),
	saturate(W1,W22,S).

% seven_to_end(+Cls,-Ordered):- Ordered ugyanaz a klozhalmaz, mint
% Cls, csak a 7-es tipusu klozok a legvegere vannak hozva.
seven_to_end(Cls,Ordered):-
	separate_seven(Cls,Seven,Rest),
	append(Rest,Seven,Ordered).

% separate_seven(+Cls,-Seven,-Rest):- Cls klozokbol a 7-es tipusuak pont
% a seven-beli klozok, mig a tobbi Rest-ben van.
separate_seven([],[],[]).
separate_seven([C|Cls],[C|Seven],Rest):-
	C = [7-_,_], !,
	separate_seven(Cls,Seven,Rest).
separate_seven([C|Cls],Seven,[C|Rest]):-
	separate_seven(Cls,Seven,Rest).	

/********************************saturation*****************************/

% saturate(+W1,+W2,-S): W1 es W2 "rendezett klozok" listaja, S-t W1 es W2
% telitesevel nyerjuk ugy hogy W1 es W2-beli klozokat, valamint W2-W2-beli
% klozokat rezolvalunk egymassal
saturate(W1,[],W1).
saturate(W1,[C|W2],S):-
	redundant(C,W1), !,
	nl, print('---- ') ,print(C), print('---- redundans'),
	saturate(W1,W2,S).
saturate(W1,[C|W2],S):-
	nl, print(C),
	findall(R,(
		   resolve_list(C,W1,Res),
		   simplifyClause(Res,Res1),
		   cls_to_ocls(Res1,R),
		   nl, print('  = '), print(R),
		   true
		  ), Rs),
	elim_reds(W1,C,EW1),
	append(Rs,W2,EW2),
	saturate([C|EW1],EW2,S).


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

/******************* Alap szuperpozicios kovetkeztetesi lepes *********************/

% resolve_list(+C,+W,-R):- C klozt rezolvalva 1 vagy tobb W beli klozzal kapjuk R-et
resolve_list([7-N,Cls],W,Res):- !,
	Cls = [not(arole(R,_,_))|_],
	findall(C, (
		     C1 = [3,C2],
		     C2 = [atleast(_,arole(R,_,_),_)|_],
		     member(C1,W),
		   copy_term(C2,C)
		   ), Three
	       ),
	sublist_max(Three,N,Size,SomeThree),
	copy_term(Cls,Cls1),
	hyperresolve(Cls1,SomeThree,Res1),
	nl, print('  + '), print(SomeThree),
	(
	  Size > 0,
	  Res = [5,Res1]
	; Size is N-1,
 	  D = [4,[atleast(_,arole(R,X,Y),_)|Res2]],
	  member(D,W),
	  binary_resolve(Res1,1,arole(R,X,Y),Res3),
	  append(Res2,Res3,Res4),
	  (
	    contains_struct(Res4,fun(_,fun(_,_))) -> Res = [6,Res4]
	  ; Res = [5,Res4]
	  )
	).
	  
	
resolve_list([5-_,_],_,_):- !, fail.

resolve_list(C,W,R):-
	member(D,W),
	copy_term(C,C1),
	copy_term(D,D1),
	resolve(C1,D1,R),
	% nl, print('  + '), print(D),
	% nl, print('  = '), print(R),
	true.

% sublist_max(+Ls,+Max,-N,-Rs):- Rs 3-as tipusu reszlistaja Ls-nek es
% az elemekben a szamossagkorlatozasok osszege N, mely nem nagyobb Max-nal
sublist_max([],_,0,[]):- !.
sublist_max(_,Max,0,[]):-
	Max =< 0, !.
sublist_max([L|Ls],Max,N,[L|Rs]):-
	L = [atleast(N2,_,_)|_],
	Max1 is Max - N2,
	sublist_max(Ls,Max1,N1,Rs),
	N is N1 + N2.
sublist_max([_|Ls],Max,N,Rs):-
	sublist_max(Ls,Max,N,Rs).

% hyperresolve(+C,+W,-Res):-
% W-ben 3-as tipusu klozok vannak, melyeket hyperrezolvaljuk C negalt binaris literaljaival,
% melyek C elejen vannak. Az eredo kloz Res.
% A klozok itt nem tartalmazzak a tipusukat
hyperresolve(Seven,[],Seven).
hyperresolve(Seven,[[atleast(N,arole(R,X,Y),C)|Rest]|W],Res):-
	binary_resolve(Seven,N,arole(R,X,Y),SevenRest),
	equality_superpose(SevenRest,Y,C,SevenRest2),
	append(SevenRest2,Rest,SevenRest3),
	hyperresolve(SevenRest3,W,Res).


binary_resolve([not(arole(R,X,Y))|Rest1],N,arole(R,X,Y),Rest2):-
	N > 0, !,
	N1 is N - 1,
	binary_resolve(Rest1,N1,arole(R,X,Y),Rest2).
binary_resolve(C,_,_,C).

		      

equality_superpose([],_,_,[]).
equality_superpose([eq(A,B)|Ls],Y,C,Rs):-
	A == Y,
	B == Y,
	!,
	equality_superpose(Ls,Y,C,Rs).
equality_superpose([eq(A,B)|Ls],Y,C,[R|Rs]):-
	(
	  A == Y -> To = B
	; B == Y -> To = A
	), !,
	(
	  C = aconcept(CName,_) -> R = aconcept(CName,To)
	; C = not(aconcept(CName,_)) -> R = not(aconcept(CName,To))
	; C = nconcept(CName,_) -> R = nconcept(CName,To)
	; C = not(nconcept(CName,_)) -> R = not(nconcept(CName,To))
	),
	equality_superpose(Ls,Y,C,Rs).
equality_superpose([L|Ls],Y,C,[L|Rs]):-
	equality_superpose(Ls,Y,C,Rs).



% resolve(+C1,+C2,-Res): Res a C1 ‚és C2 klozok rezolvense
resolve([1,[not(R),S]],[T,[atleast(N,R,C)|Rest]],[Type,[atleast(N,S,C)|Rest]]):-
	( T = 3; T=4), !,
	
	(
	  contains_struct(S,arole(_,fun(_,_),_)) -> Type = 4
	; Type = 3
	).
resolve([T,C1],[1,C2],Res):- !,
	( T = 3; T=4), !,
	resolve([1,C2],[T,C1],Res).

resolve([3,[atleast(_,_,C)|Rest1]],[5,[NC|Rest2]],[5,Res]):-
	( C = not(NC), !; NC = not(C), !),
	append(Rest1,Rest2,Res).
resolve([5,C1],[3,C2],Res):-
	resolve([3,C2],[5,C1],Res).

resolve([5,[Atom|Rest1]],[5,[not(Atom)|Rest2]],[5,Res]):- !,
	append(Rest1,Rest2,Res).
resolve([5,[not(Atom)|Rest1]],[5,[Atom|Rest2]],[5,Res]):- !,
	append(Rest1,Rest2,Res).

resolve([T1,[C1|Rest1]],[T2,[C2|Rest2]],[N,Res]):-
	( T1 = 5; T1 = 6), !,
	( T2 = 5; T2 = 6), !,
	(
	  C1 = not(C2)
	; C2 = not(C1)
	), !,
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,fun(_,fun(_,_))) -> N = 6
	; N = 5
	).

/*
% decomposeClause(+Cls,-Decomposed)
% Cls kloz esetleges dekompoziciojabol kapott klozlista Decomposed
decomposeClause(Cls,Decomposed):-
	select(arole(R,Arg1,Arg2),Cls,Rest),
	nonvar(Arg2),
	Arg2 = marked(fun(F,Arg1)), !,
	( contains_struct(Rest,fun(_,_)) -> atom_concat('d_',R,Name1),
	                                    atom_concat(Name1,F,Name),
	                                    sort([nconcept(Name,Arg1)|Rest],D1),
	                                    cls_to_ocls(D1,D11),
	                                    D2 = [not(nconcept(Name,X)),arole(R,X,marked(fun(F,X)))],
	                                    cls_to_ocls(D2,D22),
	                                    Decomposed = [D11,D22]
	; Decomposed = [Cls]
	).
decomposeClause(Cls,Decomposed):-
	select(arole(R,Y,X),Cls,Rest),
	var(X), nonvar(Y), Y = marked(fun(F,X)), !,
	( contains_struct(Rest,fun(_,_)) -> atom_concat('dinv_',R,Name1),
	                                    atom_concat(Name1,F,Name),
	                                    sort([nconcept(Name,X)|Rest],D1),
	                                    cls_to_ocls(D1,D11),	    
	                                    D2 = [not(nconcept(Name,X)),arole(R,marked(fun(F,X)),X)],
	                                    cls_to_ocls(D2,D22),	    
	                                    Decomposed = [D11,D22]
	; Decomposed = [Cls]
	).
decomposeClause(Cls,[Cls]).
*/
	
	
simplifyClauses([],[]).
simplifyClauses([Cls|Clauses],[R|Rs]):-
	simplifyClause(Cls,R),
	simplifyClauses(Clauses,Rs).

simplifyClause([T,Cls],[T,Simplified]):-
	arrangeEquality(Cls,Arranged),
	( member(eq(X,Y),Arranged), X == Y, !, Simplified = [true]
	; member(true,Arranged), !, Simplified = [true]
	; member(eq(X1,Y1),Arranged), member(not(eq(X2,Y2)),Arranged), identical_terms(X1,X2), identical_terms(Y1,Y2), !, Simplified = [true]
	; member(aconcept(Q,B1),Arranged), member(not(aconcept(Q,B2)),Arranged), identical_terms(B1,B2), !, Simplified = [true]
	; member(nconcept(Q,B1),Arranged), member(not(nconcept(Q,B2)),Arranged), identical_terms(B1,B2), !, Simplified = [true]
	; member(arole(R,X1,Y1),Arranged), member(not(arole(R,X2,Y2)),Arranged), identical_terms(X1,X2), identical_terms(Y1,Y2), !, Simplified = [true]
	; sort(Arranged,S1),
	  delete(S1, not(true),S2),
	  delete_trivial_equals(S2,Simplified)
	).

% identical_terms(+T1,+T2)
% T1 es T2 azonos termek, legfeljebb megjeloltsegben kulonboznek
identical_terms(T1,T2):-
	T1 == T2, !.
identical_terms(T1,T2):-
	var(T1), !, T1 == T2.
identical_terms(T1,T2):-
	var(T2), !, T1 == T2.
identical_terms(T1,marked(T2)):-
	!, identical_terms(T1,T2).
identical_terms(marked(T1),T2):-
	!, identical_terms(T1,T2).
identical_terms(fun(F1,T1),fun(F2,T2)):-
	F1 == F2,
	identical_terms(T1,T2).

	
% delete_trivial_equals(+Ls,-Rs)
%	Ls klozbol elhagyva az X=X alaku trivialitasokat kapjuk Rs -et
delete_trivial_equals([],[]).
delete_trivial_equals([not(eq(X,Y))],[true]):-
	(	X == Y
	;	X == marked(Y)
	; Y == marked(X)
	), !.	
delete_trivial_equals([not(eq(X,Y))|Ls],Rs):-
	(	X == Y
	;	X == marked(Y)
	; Y == marked(X)
	),
	!,
	delete_trivial_equals(Ls,Rs).
delete_trivial_equals([L|Ls],[L|Rs]):-
	delete_trivial_equals(Ls,Rs).
	
	
	
arrangeEquality([],[]).
arrangeEquality([eq(X,Y)|L],[A|R]):-
	!,
	(	greater(X,Y) -> A = eq(X,Y)
	;	                A = eq(Y,X)
	),
	arrangeEquality(L,R).
arrangeEquality([not(eq(X,Y))|L],[A|R]):-
	!,
	( greater(X,Y) -> A = not(eq(X,Y))
	;                 A = not(eq(Y,X))
	),
	arrangeEquality(L,R).
arrangeEquality([A|L],[A|R]):-
	arrangeEquality(L,R).
	
	
	
	
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