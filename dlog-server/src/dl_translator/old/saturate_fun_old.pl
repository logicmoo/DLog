:- module(saturate_fun_old,[saturate_fun/2]).

:- use_module('../show').
:- use_module('../struct').
:- use_module(saturate_old).
:- use_module(saturate_without_binary_old).
:- use_module(selectResolvable_old).
:- use_module(library(lists),[append/3,member/2,select/3]).
:- use_module(library(terms), [term_variables/2]).


/***************************** Klozhalmaz telitese **********************************/

% saturate_fun(+W,-S):-
% W klozok listaja, melyek telitesevel kapjuk S-et
% ugy hogy minden kovetkeztetest elvegzunk a fuggvenyjelekkel
saturate_fun(W,S):-
	simplifyClauses(W,W2),	
	% saturate_fun([],W2,Saturated),

	% omit_structs(Saturated, eq(marked(fun(_,_)),_),R1),
	% omit_structs(R1, not(eq(fun(_,_),_)), R2),

	% nl, nl, print('Fuggveny egyenloseg kikuszobolese utan'), nl,
	% show(R2), nl,nl,

	% saturate(R2,R3),

	% nl, nl, print('Fuggveny egyenloseg kikuszobolese utan - telitve'), nl,
	% show(R3), nl,nl,

	omit_structs(W2, arole(_,X,fun(_,X)),R4),
	omit_structs(R4, arole(_,X,marked(fun(_,X))),R5),
	omit_structs(R5, arole(_,marked(fun(_,_)),_),R6),

	% nl, nl, print('Fuggvenyjeles szerepek elhagyasa utan'), nl,
	% show(R6), nl,nl,
	
	
	saturate_without_binary(R6,R7),
	saturate(R7,R8),

	omit_structs(R8, fun(_,_),FunFree),

	% nl, nl, print('Fuggvenyek teljes kikuszobolese utan'), nl,
	% show(FunFree), nl,nl,

	remove_redundant(FunFree,S).
	
	
	
% saturate_fun(+W1,+W2,-S): W1 es W2 "rendezett klozok" listaja, S-t W1 es W2
% telitesevel nyerjuk ugy hogy W1 es W2-beli klozokat, valamint W2-W2-beli
% klozokat rezolvalunk egymassal
saturate_fun(W1,[],W1).
saturate_fun(W1,[C|W2],S):-
	redundant(C,W1), !,	
	% print('---- ') ,print(C), print('---- redundans'),nl,
	saturate_fun(W1,W2,S).
saturate_fun(W1,[C|W2],S):-	
	% print(C), nl,
	findall(R,(
		   member(R1,W1),				
		   resolve_fun(R1,C,Res),
		   % \+ redundant(Res,W1),
		   decomposeClause(Res,Decomposed),
		   member(R,Decomposed),
		   % print('   + '), print(R1),nl,
		   % print('   = '), print(R),nl,
		   true
		   ), Rs),
	elim_reds(W1,C,EW1),
	append(Rs,W2,EW2),
	saturate_fun([C|EW1],EW2,S).


/******************* Alap szuperpozicios kovetkeztetesi lepes *********************/

% resolve_fun(+C1,+C2,-R): R a C1 ‚és C2 klozok rezolvense
resolve_fun(C1,C2,R):-
	copy_term(C1,R1), 
	copy_term(C2,R2),

	( select(Res1,R1,Tail1), select(Res2,R2,Tail2)
	; select(Res1,R2,Tail1), select(Res2,R1,Tail2)
	),
	Res1 = eq(marked(X),Y),
	nonvar(X), X = fun(F,_),
	\+ (
	     member(Greater_eq,Tail1),
	     Greater_eq = eq(marked(GX),_),
	     nonvar(GX), GX = fun(G,_),
	     G @> F
	   ),

	( var(Y), !
	; Y = marked(fun(FY,_))
	),
	( Res2 = not(eq(Arg1,Arg2)),
	    nonvar(Arg1), nonvar(Arg2),
	    Arg1 = fun(F,_),
	    Arg2 = fun(FY,_)
	; contains_struct(Res2,fun(F,_)),
	    \+ contains_struct([Res2|Tail2], not(eq(fun(F,_),_)))
	),
	find_unifiable(Res2,X),
	L1 = [Res2|Tail2],
	replace_occurrence_list(L1,X,Y,L2),
	append(Tail1,L2,L3),
	simplifyClause(L3,R).
	
	
/*********************** fuggvenyjelek kiiktatasa *****************/
	
	% remove_functions_list(+L,+NoFun,-R)
remove_functions_list([],_,[]).
remove_functions_list([L|Ls],NoFun,[R|Rs]):-
	remove_functions(L,NoFun,R),
	remove_functions_list(Ls,NoFun,Rs).

remove_functions(L,NoFun,R):-
	separate_functions(L,Funs,Rest),
	term_variables(Funs,Vars),
	findall(Preds, (
			 member(X,Vars),
			 contains_struct2(Funs,fun(F,_)),
			 findall(Pred, (
					 member(C,Funs),
					 ( C = aconcept(Name,fun(F,X)) -> Pred = aconcept(Name,X)
					 ; C = nconcept(Name,fun(F,X)) -> Pred = nconcept(Name,X)
					 ; C = not(aconcept(Name,fun(F,X))) -> Pred = not(aconcept(Name,X))
					 ; C = not(nconcept(Name,fun(F,X))) -> Pred = not(aconcept(Name,X))
					 ; C = aconcept(Name,marked(fun(F,X))) -> Pred = aconcept(Name,X)
					 ; C = nconcept(Name,marked(fun(F,X))) -> Pred = nconcept(Name,X)
					 ; C = not(aconcept(Name,marked(fun(F,X)))) -> Pred = not(aconcept(Name,X))
					 ; C = not(nconcept(Name,marked(fun(F,X)))) -> Pred = not(aconcept(Name,X))
					 )
				       ), Preds
				)			 
			 ), PredGroups
	       ),
	( contradictable_list(PredGroups,NoFun) -> R = Rest
	; R = [true]
	).

separate_functions([],[],[]).
separate_functions([L|Ls],[L|Funs],Rest):-
	contains_struct(L,fun(_,_)), !,
	separate_functions(Ls,Funs,Rest).
separate_functions([L|Ls],Funs,[L|Rest]):-
	separate_functions(Ls,Funs,Rest).

contradictable_list([],_).
contradictable_list([P|Preds],NoFun):-
	contradictable(P,NoFun),
	contradictable_list(Preds,NoFun).

contradictable(P,NoFun):-
	selectResolvable([P|NoFun],Ordered),
	saturate(Ordered,Saturated),
	member([],Saturated), !.