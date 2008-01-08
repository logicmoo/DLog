:- module(saturate,[saturate/2, resolve/3, redundant/2, remove_redundant/2, elim_reds/3, simplifyClauses/2, simplifyClause/2, find_unifiable/2, replace_occurrence_list/4, decomposeClause/2, remove_temp/2]).

:- use_module(show).
:- use_module(struct).
:- use_module(selectResolvable).
:- use_module(library(lists),[append/3,member/2,select/3,reverse/2,delete/3]).
:- use_module(library(terms),[subsumes/2, term_variables/2]).
:- use_module(library(ordsets),[list_to_ord_set/2,ord_subset/2]).



/***************************** Klozhalmaz telitese **********************************/

% saturate(+W,-S):-
% W klozok listaja, melyek telitesevel kapjuk S-et
saturate(W,S):-
	simplifyClauses(W,W2),
	selectResolvable(W2,Selected),
	separate(Selected,NegBin,OrigIneq,FunOrig,FunDeriv,Rest),
	append(NegBin,FunOrig,A1),
	append(A1,FunDeriv,A2),
	append(A2,OrigIneq,A3),
	append(A3,Rest,Arranged),

	append(FunOrig,OrigIneq,AllOrig),
	saturate([],Arranged,OrigIneq,AllOrig,S).

% saturate(+W1,+W2,-S): W1 es W2 "rendezett klozok" listaja, S-t W1 es W2
% telitesevel nyerjuk ugy hogy W1 es W2-beli klozokat, valamint W2-W2-beli
% klozokat rezolvalunk egymassal
% OrigIneq g(x) != f(x) kifejezeseket tartalmazzo klozok listaja
% FunOrig az osszes tobbi, jeloletlen fuggvenyt tartalmazo kloz listaja
saturate(W1,[],_,_,W1).
saturate(W1,[C|W2],OrigIneq,AllOrig,S):-
	redundant(C,W1), !,
	% print('---- ') ,print(C), print('---- redundans'),nl,
	saturate(W1,W2,OrigIneq,AllOrig,S).
saturate(W1,[C|W2],OrigIneq,AllOrig,S):-
	% length(W1,L1),
	% length(W2,L2),
	% print(L1), print(--), print(L2), print('  '), nl,
	% print(C), nl,
	findall(R,(
		   member(R1,W1),
		   resolve(R1,C,Res),
		   
		   \+ thereIsAnotherFun(C,R1,OrigIneq),
		   
		   saturate_fun_equal(Res,AllOrig,ResList),
		   member(Res2,ResList),
		   % \+ redundant(Res2,W1),
		   decomposeClause(Res2,Decomposed),		   
		   member(R,Decomposed),
		   % print('   + '), print(R1),nl,		   
		   % print('   = '), print(R),nl,
		   true
		  ), Rs),
	elim_reds(W1,C,EW1),
	append(Rs,W2,EW2),
	saturate([C|EW1],EW2,OrigIneq,AllOrig,S).



/*********************** Redundans klozok elhagyasa *******************************/

% redundant(+C, +Clauses): igaz, ha van C-nel szukebb kloz a Clauses
% klozhalmazban
% nincs behelyettesites
redundant(C,_):-
	member(true,C), !.
	
/*
redundant(C,Clauses):-
	member(General,Clauses),
	length(General,LG),
	length(C,LC),
	LC >= LG,
	redundant_clause(C,General), !.
*/

redundant(C,Clauses):-
	member(General,Clauses),
	length(General,LG),
	length(C,LC),
	LC >= LG,
	copy_term(General,General2),
	copy_term(C,C2),
	remove_marker(General2,General3),
	remove_marker(C2,C3),
	term_variables(C2,Vars),
	includes(C3,General3),
	none_related(Vars), !.
	
redundant_clause(_,[]).
redundant_clause(R,C):-
	copy_term(R,R1),
	copy_term(C,C1),
	remove_marker(R1,R2),
	remove_marker(C1,C2),
	term_variables(C2,TC),
	terms_clause(R2,TR1),
	sort(TR1,TR),
	% list_to_ord_set(R2,R3),
	% list_to_ord_set(C2,C3),	
	substitute_terms(TC,TR),
	all_member(C2,R2), !.
	% ord_subset(C3,R3), !.

all_member([],_).
all_member([L|Ls],R):-
	select(M,R,R2), L == M,
	all_member(Ls,R2).

terms_clause([],[]).
terms_clause([L|Ls],Ts):-
	terms_literal(L,T),
	append(T,Ts2,Ts),
	terms_clause(Ls,Ts2).
terms_literal(not(L),Ts):-
	terms_literal(L,Ts).
terms_literal(arole(_,X,Y),Ts):-	
	terms(X,TX),
	terms(Y,TY),
	append(TX,TY,Ts).
terms_literal(aconcept(_,X),Ts):-	
	terms(X,Ts).
terms_literal(nconcept(_,X),Ts):-
	terms(X,Ts).
terms(X,[X]):-
	( var(X)
	; atom(X)
	), !.
terms(fun(F,X),[fun(F,X)|Ts]):-
	terms(X,Ts).

substitute_terms([],_).
substitute_terms([T|Ts],TR):-
	member(T,TR),
	substitute_terms(Ts,TR).


% includes(+C1,+C2): C1 kloz tartalmazza C2-t
% !!! behelyettesites tortenik !!!		  
includes(_,[]).
includes(C1,[A|C2]):-	
	select(B,C1,Rest1),
	subsumes(A,B),
	% A = B,
	includes(Rest1,C2).
	

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
	(	A == B	->	fail
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

% resolve(+C1,+C2,-R): R a C1 ‚és C2 klozok rezolvense
resolve(C1,C2,R):-
	copy_term(C1,R1), R1 = [Res1|Tail1],
	copy_term(C2,R2), R2 = [Res2|Tail2],		
	( Res1 = not(Res11), \+ Res11 = eq(_,_) -> unify(Res11,Res2)
	; Res2 = not(Res22), \+ Res22 = eq(_,_) -> unify(Res1,Res22)
	),
	!,	
	append(Tail1,Tail2,L1),	
	simplifyClause(L1,L2),
	cls_to_ocls(L2,R).	

resolve(C1,C2,R):-
	copy_term(C1,R1),
	copy_term(C2,R2),
	( R1=[eq(X,Y)|Cls1], R2=[Res|Cls2]
	; R2=[eq(X,Y)|Cls1], R1=[Res|Cls2]
	),
	nonvar(X),
/*
	( Res = not(eq(Arg1,Arg2)),
	    nonvar(Arg1), nonvar(Arg2),
	    Arg1 = fun(F1,_), Arg2 = fun(F2,_), !,
	    nonvar(Y),
	    X = marked(fun(F1,_)),
	    Y = marked(fun(F2,_))
	; true
	),
*/
	find_unifiable(Res,X),
	L1 = [Res|Cls2],
	replace_occurrence_list(L1,X,Y,L2),
	append(Cls1,L2,L3),
	simplifyClause(L3,L4),
	cls_to_ocls(L4,R).
		
unify(L1,L2):-
	( var(L1) ->
	    ( var(L2) -> L1 = L2
	    ; L2 = marked(L3) -> L1 = L2
	    ; L1 = marked(L2)
	    )
	; var(L2) ->
	    ( L1 = marked(L3) -> L2 = L1
	    ; L2 = marked(L1)
	    )
	; L1 = marked(Lm1), !, unify(Lm1,L2)
	; L2 = marked(Lm2), !, unify(L1,Lm2)
	; atomic(L1) ->
	    ( L2 == L1
	    ; L2 == marked(L1)
	    )
	; atomic(L2) -> L1 == marked(L2)
	; L1 =.. U1, L2 =.. U2, !, unifyList(U1,U2)
	).

unifyList([],[]).
unifyList([A1|L1],[A2|L2]):-
	unify(A1,A2),
	unifyList(L1,L2).
		

%	find_unifiable(+L,+C): L kifejezesnek van C-vel egyesitheto resze
% nem valtozo pozicioban
% a hivas soran az egyesites el is vegzodik
find_unifiable(L,C):-
	(	var(L), !, fail
	;	L = marked(_), !, fail
	;	unify(L,C), !
	;	L =.. [_|U], find_unifiable_list(U,C)
	).

%	find_unifiable_list(+L,+C): L kifejezesek listaja, melyek
% egyikenek van C-vel egyesitheto resze
% a hivas soran az egyesites el is vegzodik
find_unifiable_list([L|Ls],C):-
	(	find_unifiable(L,C), !
	;	find_unifiable_list(Ls,C)
	).

% replace_occurrence_list(+L,+X,+Y,-R),
%	L kifejezesek listaje, melyek mindegyikeben
%	X -et kicserelve Y-re kapjuk R-et.
replace_occurrence_list([],_,_,[]).
replace_occurrence_list([L|Ls],X,Y,[R|Rs]):-
	replace_occurrence(L,X,Y,R),
	replace_occurrence_list(Ls,X,Y,Rs).

% replace_occurrence(+L,+X,+Y,-R),
%	L kifejezesben	X -et kicserelve Y-re kapjuk R-et.
replace_occurrence(L,X,Y,R):-
	( var(L), !, R = L
	;	L == X, !, R = Y
	; marked(L) == X, !, R = Y
	; atomic(L), !, R = L
	;	L =..[Head|U], replace_occurrence_list(U,X,Y,V), R =.. [Head|V]
	).

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

	
	
simplifyClauses([],[]).
simplifyClauses([Cls|Clauses],[R|Rs]):-
	simplifyClause(Cls,R),
	simplifyClauses(Clauses,Rs).

simplifyClause(Cls,Simplified):-
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
%	L klozok listaja, melyeket telitve a bevezetett fogalmak szerint
%	es elhagyva ezen fogalmakat tartalmazo klozokat kapjuk R klozlistat
%	ha vegtelen ciklusba esnenk, akkor meghagyjuk az adott bevezetett fogalmat
remove_temp(L,R):-
	contains_struct2(L,nconcept(X,_)),
	\+ (
	     member(Cls,L),
	     member(nconcept(X,_),Cls),
	     member(not(nconcept(X,_)),Cls)
	   ),
	!,
	% write('removing: '), write(X), nl,
	saturate_specific(L,X,L2),
	omit_structs(L2,nconcept(X,_),L3),	
	remove_temp(L3,R).
remove_temp(L,L).


% saturate_specific(+L,+PredName,-R)
%	L klozok listaja, melyeket telitve a PredName nevu bevezetett fogalom szerint
%	kapjuk R klozlistat
saturate_specific(L,PredName,R):-
	saturate_specific([],L,PredName,R).
	
	
% saturate_specific(+W1,+W2, +PredName, -R): W1 es W2 klozok listaja, R-t W1 es W2
% telitesevel nyerjuk a PredName nevu bevezetett fogalom szerint
% ugy hogy W1 es W2-beli klozokat, valamint W2-W2-beli klozokat rezolvalunk egymassal
saturate_specific(W1,[],_,W1).
saturate_specific(W1,[C|W2],PredName,R):-	
	redundant(C,W1), !, 
	% print('---- ') ,print(C), print('---- redundans'),nl,
	saturate_specific(W1,W2,PredName,R).
saturate_specific(W1,[C|W2],PredName,S):-	
	% print(C), nl,
	findall(R,(
				member(R1,W1),
				resolve_specific(R1,C,PredName,R),
				% print('  + '), print(R1),nl,
				% print('    = '), print(R),nl,
				\+ redundant(R,W1)
		  ),Rs),
	elim_reds(W1,C,EW1),	
	append(Rs,W2,EW2),
	saturate_specific([C|EW1],EW2,PredName,S).		


% resolve_specific(+Clause1,Clause2,PredName,Resolvant): Resolvant Clause1 es Clause2 kloz PredName uj
% fogalom szerinti rezolvense
resolve_specific(C1,C2,PredName,R):-
	copy_term(C1,Res1),
	copy_term(C2,Res2),
	(	select(nconcept(PredName,X),Res1,M1),		
		select(not(nconcept(PredName,X)),Res2,M2)		
	;	select(not(nconcept(PredName,X)),Res1,M1),		
		select(nconcept(PredName,X),Res2,M2)		
	),
	append(M1,M2,L),sort(L,L1),
	simplifyClause(L1,R).	



% remove_redundant(+L,-R),
% L klozlistabol kikuszobolve a redundansakat kapjuk R klozlistat
remove_redundant(L,R):-
	remove_redundant(L,[],R).


remove_redundant([],R,R).
remove_redundant([L|Ls],Acc,R):-
	redundant(L,Acc), !,
	remove_redundant(Ls,Acc,R).
remove_redundant([L|Ls],Acc,R):-
	elim_reds(Acc,L,Acc2),
	remove_redundant(Ls,[L|Acc2],R).






% saturate_fun_equal(+C,+W,-CList)
% Ha C-ben van f(x) = z alaku kloz, akkor azt szuperponaljuk
% az osszes lehetseges modon W klozaiba. Az eredmeny CList
saturate_fun_equal(C,W,CList):-
	select(eq(X,Y),C,Rest),
	nonvar(X), !,
	findall(R, (
		     member(C1,W),
		     resolve([eq(X,Y)|Rest],C1,R)
		   ), Rs
	       ),
	saturate_fun_equal_list(Rs,W,CList).
saturate_fun_equal(C,_,[C]).

saturate_fun_equal_list([],_,[]).
saturate_fun_equal_list([C|Cs],W,RList):-
	saturate_fun_equal(C,W,R),
	append(R,RList2,RList),
	saturate_fun_equal_list(Cs,W,RList2).



% thereIsAnotherFun(+C1,+C2,-W)
% C1 es C2 rezolvalhato binaris literal szerint, de nem erdemes
thereIsAnotherFun([not(arole(R,_,_))|Rest],[arole(R,_,fun(F,_))|_],W):-	
	contains_struct2(W,not(eq(fun(G,_),fun(F,_)))), !,
	\+ contains_struct(Rest,fun(G,_)),
	\+ contains_struct(Rest,fun(F,_)).
thereIsAnotherFun([arole(R,_,fun(F,_))|_],[not(arole(R,_,_))|Rest],W):-	
	contains_struct2(W,not(eq(fun(G,_),fun(F,_)))), !,
	\+ contains_struct(Rest,fun(G,_)),
	\+ contains_struct(Rest,fun(F,_)).


/***************************************************/
/************** markerek eltuntetese ***************/
/***************************************************/
% remove_marker(+Clause,-Result): Result klozt ugy kapjuk,
% hogy Clause klozbol kitoroljuk a markereket
remove_marker([],[]).
remove_marker([L|Ls],[R|Rs]):-
	remove_marker_literal(L,R),
	remove_marker(Ls,Rs).

remove_marker_literal(X,X):-
	( atomic(X)
	; var(X)
	), !.
remove_marker_literal(marked(X),X) :- !.
remove_marker_literal(not(X),not(R)):-
	remove_marker_literal(X,R).
remove_marker_literal(arole(R,X,Y),arole(R,X1,Y1)):-
	!,
	remove_marker_literal(X,X1),
	remove_marker_literal(Y,Y1).
remove_marker_literal(aconcept(C,X),aconcept(C,X1)):-
	!,
	remove_marker_literal(X,X1).
remove_marker_literal(nconcept(C,X),nconcept(C,X1)):-
	!,
	remove_marker_literal(X,X1).
remove_marker_literal(eq(X,Y),eq(X1,Y1)):-
	!,
	remove_marker_literal(X,X1),
	remove_marker_literal(Y,Y1).
remove_marker_literal(fun(f,X),fun(f,Y)):-
	!,
	remove_marker_literal(X,Y).


/***************************************************/
/*************** klozok szeparalasa ****************/
/***************************************************/
% separate(+Cs,-NegBin,-OrigIneq,-FunOrig,-FunDeriv,-Rest):-
separate([],[],[],[],[],[]).
separate([C|Cs],[C|NegBin],OrigIneq,FunOrig,FunDeriv,Rest):-
	contains_struct(C,not(arole(_,_,_))), !,
	separate(Cs,NegBin,OrigIneq,FunOrig,FunDeriv,Rest).
separate([C|Cs],NegBin,[C|OrigIneq],FunOrig,FunDeriv,Rest):-
	contains_struct(C,not(eq(fun(_,_),fun(_,_)))), !,
	separate(Cs,NegBin,OrigIneq,FunOrig,FunDeriv,Rest).
separate([C|Cs],NegBin,OrigIneq,[C|FunOrig],FunDeriv,Rest):-
	contains_struct(C,fun(_,_)),
	\+ contains_struct(C,marked(_)), !,
	separate(Cs,NegBin,OrigIneq,FunOrig,FunDeriv,Rest).
separate([C|Cs],NegBin,OrigIneq,FunOrig,[C|FunDeriv],Rest):-
	contains_struct(C,fun(_,_)), !,
	separate(Cs,NegBin,OrigIneq,FunOrig,FunDeriv,Rest).
separate([C|Cs],NegBin,OrigIneq,FunOrig,FunDeriv,[C|Rest]):-
	separate(Cs,NegBin,OrigIneq,FunOrig,FunDeriv,Rest).

