:- module(saturate_old,[saturate/2, resolve/3, redundant/2, remove_redundant/2, elim_reds/3, simplifyClauses/2, simplifyClause/2, remove_temp/2]).
:- use_module('../show').
:- use_module('../struct').
:- use_module(selectResolvable_old).
:- use_module(library(lists),[append/3,member/2,select/3,reverse/2,delete/3]).
% :- use_module(library(terms),[subsumes/2, term_variables/2]).
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
% OrigIneq g(x) != f(x) kifejezeseket tartalmazzo klozok listaja
% FunOrig az osszes tobbi, jeloletlen fuggvenyt tartalmazo kloz listaja
saturate(W1,[],W1).
saturate(W1,[C|W2],S):-
	redundant(C,W1), !,
	% nl, print('---- ') ,print(C), print('---- redundans'),
	saturate(W1,W2,S).
saturate(W1,[C|W2],S):-
	% nl, print(C),
	findall(R,(
		   resolve_list(C,W1,Res),
		   simplifyClause(Res,Res1),
		   cls_to_ocls(Res1,R),
		   % nl, print('  = '), print(R),
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
redundant([_,C],_):-
	member(true,C), !.

redundant([_,C],Clauses):-
	member([_,D],Clauses),
	length(D,LD),
	length(C,LC),
	LC >= LD,
	copy_term(D,D2),
	copy_term(C,C2),
	term_variables(C2,Vars),
	includes(C2,D2),
	none_related(Vars), !.

% includes(+C,+D): C kloz tartalmazza D-t
% !!! behelyettesites tortenik !!!		  
includes(_,[]).
includes(C,[LD|D]):-
	select(LC,C,RestC),
	subsumes(LD,LC),
	includes(RestC,D).
	

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
		     C1 = [Type,C2],
		     C2 = [arole(R,_,_)|_],
		     member(C1,W),
		     ( Type = 3 ; Type = 30 ),
		     copy_term(C2,C)
		   ), Three
	       ),
	sublist_max(Three,N,SomeThree),
	copy_term(Cls,Cls1),
	binary_resolve_all(Cls1,SomeThree,Res1),
	(
	  \+ (Res1 == Cls1),
	  Res = [50,Res1]
	; length(SomeThree,L),
	  L is N-1,
	  D = [4,D2],
	  D2 = [arole(R,_,_)|_],
	  member(D,W),
	  copy_term(D2,D3),
	  copy_term(Cls,Cls2),
	  binary_resolve_all(Cls2,[D3|SomeThree],Res2),
	  (
	    contains_struct(Res,fun(_,fun(_,_))) -> Res = [6,Res2]
	  ; Res = [50,Res2]
	  )
	).
	  
	
resolve_list([50-_,_],_,_):- !, fail.

resolve_list(C,W,R):-
	member(D,W),
	copy_term(C,C1),
	copy_term(D,D1),
	resolve(C1,D1,R),
	% nl, print('  + '), print(D),
	% nl, print('  = '), print(R),
	true.

% sublist_max(+Ls,+N,-Rs):- R reszlistaja L-nek es hossza max N (N pozitiv egesz!!!)
sublist_max(_,1,[]).
sublist_max(Ls,1,[L]):- !,
	member(L,Ls).
sublist_max(Ls,N,Rs):-
	N1 is N-1,
	sublist_max(Ls,N1,Rs).
sublist_max(Ls,N,Rs):-
	sublist_exactly(Ls,N,Rs).

% sublist_exactly(+Ls,+N,-Rs):- R reszlistaja L-nek es pont N hosszu
sublist_exactly(Ls,N,Ls):-
	length(Ls,N), !.
sublist_exactly([L|Ls],N,[L|Rs]):-
	N1 is N-1,
	sublist_exactly(Ls,N1,Rs).
sublist_exactly([_|Ls],N,Rs):-
	sublist_exactly(Ls,N,Rs).

% binary_resolve_all(+C,+W,-Res):-
% W-ben L darab kloz van es ezek elso eleme ponalt binaris, melyeket rezolvalunk
% C elso negalt binaris elemevel.
% Az eredo kloz Res.
% A klozok itt nem tartalmazzak a tipusukat
binary_resolve_all(C,[],C).
binary_resolve_all([not(arole(R,X,Y))|Rest1],[[arole(R,X,Y)|Rest2]|W],Res):-
	append(Rest1,Rest2,Rest),
	binary_resolve_all(Rest,W,Res).


% resolve(+C1,+C2,-Res): Res a C1 ‚és C2 klozok rezolvense
resolve([1,[not(arole(R,X,Y))|Rest1]],[3,[arole(R,X,Y)|Rest2]],[N,Res]):-
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,arole(_,fun(_,_),_)) -> N = 4
	; N = 30
	).
resolve([1,[not(arole(R,X,Y))|Rest1]],[30,[arole(R,X,Y)|Rest2]],[N,Res]):-
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,arole(_,fun(_,_),_)) -> N = 4
	; N = 30
	).
resolve([3,C1],[1,C2],Res):- !,
	resolve([1,C2],[3,C1],Res).
resolve([30,C1],[1,C2],Res):- !,
	resolve([1,C2],[3,C1],Res).

	
resolve([1,[not(arole(R,X,Y))|Rest1]],[4,[arole(R,X,Y)|Rest2]],[N,Res]):-
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,arole(_,fun(_,_),_)) -> N = 4
	; N = 30
	).
resolve([4,C1],[1,C2],Res):- !,
	resolve([1,C2],[4,C1],Res).
	


resolve([30,[arole(R,X,Y)|Rest1]],[7-_,[not(arole(R,X,Y))|Rest2]],[7,Res]):-
	append(Rest1,Rest2,Res).
resolve([30,[arole(R,X,Y)|Rest1]],[50-_,[not(arole(R,X,Y))|Rest2]],[50,Res]):-
	append(Rest1,Rest2,Res).
resolve([30,[arole(R,X,Y)|Rest1]],[50,C2],[50,Res]):-
	select(not(arole(R,X,Y)),C2,Rest2),
	append(Rest1,Rest2,Res).
resolve([3,[arole(R,X,fun(F,X))|Rest1]],[50,[eq(fun(F,X),Z)|Rest2]],[10,[arole(R,X,Z)|Res]]):-
	append(Rest1,Rest2,Res).
resolve([50-N,C1],[30,C2],Res):- !,
	resolve([30,C2],[50-N,C1],Res).
resolve([50,C1],[30,C2],Res):- !,
	resolve([30,C2],[50,C1],Res).
resolve([50,C1],[3,C2],Res):- !,
	resolve([3,C2],[50,C1],Res).


resolve([3,[arole(R,X,fun(F,X))|Rest1]],[6,[eq(fun(F,X),Z)|Rest2]],[10,[arole(R,X,Z)|Res]]):-
	append(Rest1,Rest2,Res).
resolve([6,C1],[3,C2],Res):- !,
	resolve([3,C2],[6,C1],Res).

resolve([T1,[C|Rest1]],[T2,[NC|Rest2]],[50,Res]):-
	neg_atoms(C,NC),
	( T1 = 5; T1 = 50),
	( T2 = 5; T2 = 50), !,
	append(Rest1,Rest2,Res).

resolve([5,[aconcept(C,X)|Rest1]],[50,[eq(X,Z)|Rest2]],[50,[aconcept(C,Z)|Res]]):-
	append(Rest1,Rest2,Res).
resolve([5,[nconcept(C,X)|Rest1]],[50,[eq(X,Z)|Rest2]],[50,[nconcept(C,Z)|Res]]):-	
	append(Rest1,Rest2,Res).

resolve([50,[eq(X,Z)|Rest1]],[5,[aconcept(C,X)|Rest2]],[50,[aconcept(C,Z)|Res]]):-
	append(Rest1,Rest2,Res).
resolve([50,[eq(X,Z)|Rest1]],[5,[nconcept(C,X)|Rest2]],[50,[nconcept(C,Z)|Res]]):-
	append(Rest1,Rest2,Res).

resolve([5,[not(aconcept(C,X))|Rest1]],[50,[eq(X,Z)|Rest2]],[50,[not(aconcept(C,Z))|Res]]):-
	append(Rest1,Rest2,Res).
resolve([5,[not(nconcept(C,X))|Rest1]],[50,[eq(X,Z)|Rest2]],[50,[not(nconcept(C,Z))|Res]]):-
	append(Rest1,Rest2,Res).

resolve([50,[eq(X,Z)|Rest1]],[5,[not(aconcept(C,X))|Rest2]],[50,[not(aconcept(C,Z))|Res]]):-
	append(Rest1,Rest2,Res).
resolve([50,[eq(X,Z)|Rest1]],[5,[not(nconcept(C,X))|Rest2]],[50,[not(nconcept(C,Z))|Res]]):-
	append(Rest1,Rest2,Res).


resolve([5,[not(eq(X,Y))|Rest1]],[50,[eq(X,Z)|Rest2]],[50,[not(eq(Y,Z))|Res]]):-
	append(Rest1,Rest2,Res).
resolve([50,[eq(X,Z)|Rest1]],[5,[not(eq(X,Y))|Rest2]],[50,[not(eq(Y,Z))|Res]]):-
	append(Rest1,Rest2,Res).


resolve([5,[aconcept(C,X)|Rest1]],[6,[not(aconcept(C,X))|Rest2]],[N,Res]):-
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,fun(_,fun(_,_))) -> N = 6
	; N = 50
	).
resolve([5,[nconcept(C,X)|Rest1]],[6,[not(nconcept(C,X))|Rest2]],[N,Res]):-
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,fun(_,fun(_,_))) -> N = 6
	; N = 50
	).

resolve([5,[not(aconcept(C,X))|Rest1]],[6,[aconcept(C,X)|Rest2]],[N,Res]):-
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,fun(_,fun(_,_))) -> N = 6
	; N = 50
	).
resolve([5,[not(nconcept(C,X))|Rest1]],[6,[nconcept(C,X)|Rest2]],[N,Res]):-
	append(Rest1,Rest2,Res),
	(
	  contains_struct(Res,fun(_,fun(_,_))) -> N = 6
	; N = 50
	).

resolve([6,C1],[5,C2],Res):- !,
	resolve([5,C2],[6,C1],Res).
resolve([6,[C|Rest1]],[6,[NC|Rest2]],[50,Res]):-
	neg_atoms(C,NC),
	append(Rest1,Rest2,Res).

% atom_concept(+C,-Name,-Arg)
atom_concept(aconcept(C,X),C,X).
atom_concept(nconcept(C,X),C,X).

% neg_atoms(+C,?D):-
neg_atoms(C,not(C)):-
	atom_concept(C,_,_), !.
neg_atoms(not(C),C):-
	atom_concept(C,_,_).


/*
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
*/

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
%	L klozok listaja, melyeket telitve a bevezetett fogalmak szerint
%	es elhagyva ezen fogalmakat tartalmazo klozokat kapjuk R klozlistat
%	ha vegtelen ciklusba esnenk, akkor meghagyjuk az adott bevezetett fogalmat
remove_temp(L,R):-
	contains_struct2(L,nconcept(Pred,_)),
	\+ (
	     member(Cls,L),
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
	redundant_notype(C,W1), !, 
	saturate_specific(W1,W2,PredName,R).
saturate_specific(W1,[C|W2],PredName,S):-
	% print(C), nl,
	findall(R,(
		   member(R1,W1),
		   resolve_specific(R1,C,PredName,R),
		   % print('  + '),print(R1), nl,print('  = '), print(R), nl,
		   true
		  ),Rs),
	elim_reds_notype(W1,C,EW1),	
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
	simplifyClause([_,L1],[_,R]).



% remove_redundant(+L,-R),
% L klozlistabol kikuszobolve a redundansakat kapjuk R klozlistat
% a klozok tipus nelkul szerepelnek
remove_redundant(L,R):-
	remove_redundant(L,[],R).


remove_redundant([],R,R).
remove_redundant([L|Ls],Acc,R):-
	redundant_notype(L,Acc), !,
	remove_redundant(Ls,Acc,R).
remove_redundant([L|Ls],Acc,R):-
	elim_reds_notype(Acc,L,Acc2),
	remove_redundant(Ls,[L|Acc2],R).

redundant_notype(C,_):-
	member(true,C), !.

redundant_notype(C,Clauses):-
	member(D,Clauses),
	length(D,LD),
	length(C,LC),
	LC >= LD,
	copy_term(D,D2),
	copy_term(C,C2),
	term_variables(C2,Vars),
	includes(C2,D2),
	none_related(Vars), !.

elim_reds_notype([],_,[]).
elim_reds_notype([A|Rest],C,Reduced):-
	redundant_notype(A,[C]), !,
	elim_reds_notype(Rest,C,Reduced).
elim_reds_notype([A|Rest],C,[A|Reduced]):-
	elim_reds_notype(Rest,C,Reduced).
