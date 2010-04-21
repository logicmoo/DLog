:- module(dl_to_fol,[iboxToClauseList/2,hboxToClauseList/2,tboxToClauseList/2]).
:- use_module('structural',[literal_concept/1]).
:- use_module('nnf',[neg/2]).
:- use_module(library(lists)).
:- use_module('../struct').



/*****************************************************/
/**** Elsorendu logikai formulakra valo lekepezes ****/
/*****************************************************/
% iboxToClauseList(+Ibox,-ClauseList): ClauseList az inv(R,S) allitasoknak
% megfelelo elsorendu logikai klozok listaja, ahol inv(R,S) Ibox-beli
% elem
iboxToClauseList([],[]).
iboxToClauseList([inv(R,S)|Ls],[[not(role(R,X,Y)),role(S,Y,X)],[not(role(S,X,Y)),role(R,Y,X)]|Rest]):-
	iboxToClauseList(Ls,Rest).

% hboxToClauseList(+Hbox,-ClauseList): ClauseList az subrole(R,S) allitasoknak
% megfelelo elsorendu logikai klozok listaja, ahol subrole(R,S) Hbox-beli
% elem
hboxToClauseList([],[]).
hboxToClauseList([subrole(R,S)|Ls],[[not(role(R,X,Y)),role(S,X,Y)]|Rest]):-
	hboxToClauseList(Ls,Rest).

% tboxToClauseList(+DLList,-ClauseList): ClauseList az X eleme DL allitasoknak
% megfelelo elsorendu logikai klozok listaja, ahol DL DLList-beli
% elem
tboxToClauseList(DLList,ClauseList):-
	tboxToClauseList(DLList,_,ClauseList,f,_).

tboxToClauseList([],_,[],FUN,FUN).
tboxToClauseList([D|Ds],X,FOL_list,FUN,NFUN):-
	toClauseList(D,X,F_list,FUN,FUN1),
	append(F_list,Rest,FOL_list),
	tboxToClauseList(Ds,X,Rest,FUN1,NFUN).

% toClauseList(+DLExpr,X,-ClauseList,+funName,-newFunName):
% ClauseList az X eleme DLExpr kifejezes elsorendu
% logikai megfelelojinek listaja kloz formaban, ahol a fuggvenyszimbolumok
% funName es newFunName koze esnek
% DLExpr egy olyan fogalom, mely a strukturalis transzformaciobol szarmazik
toClauseList(or(L),X,ClauseList,FUN,NFUN):- !,
	(
	  select(Concept,L,LRest),
	  \+ literal_concept(Concept) ->
	  toClauseList(Concept,X,FOLClauseList,FUN,NFUN),
	  orLiteralList(LRest,X,Literals),
	  combineWithAnd(FOLClauseList,Literals,ClauseList)
	;
	  orLiteralList(L,X,Clause),
	  ClauseList = [Clause],
	  NFUN = FUN
	).
toClauseList(and(L),X,ClauseList,FUN,FUN):- !,
	andLiteralList(L,X,Literals),
	( foreach(L,Literals), foreach([L],ClauseList) do true ).
toClauseList(top,_,[[true]],FUN,FUN):- !.
toClauseList(bottom,_,[[]],FUN,FUN):- !.
toClauseList(atleast(N,R,C),X,ClauseList,FUN,NFUN):- !,
	createFunNames(N,FUN,NFUN,FunNames),
	atleastClauses(FunNames,R,C,X,ClauseList).
toClauseList(atmost(N,R,C),X,[Clause],FUN,FUN):- !,
	N1 is N + 1,
	length(Vars,N1), % create N1 variables
	atmostLiterals(Vars,R,C,X,Clause).
toClauseList(C,X,[[Literal]],FUN,FUN):-
	toLiteral(C,X,Literal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

andLiteralList([],_,[]).
andLiteralList(Ds,X,Ls):-
	select(top,Ds,Rest), !,
	andLiteralList(Rest,X,Ls).
andLiteralList(Ds,_,[]):-
	member(bottom,Ds), !.
andLiteralList(Ds,X,Ls):-
	literalList(Ds,X,Ls).

orLiteralList([],_,[]).
orLiteralList(Ds,X,Ls):-
	select(bottom,Ds,Rest), !,
	orLiteralList(Rest,X,Ls).
orLiteralList(Ds,_,[true]):-
	member(top,Ds), !.
orLiteralList(Ds,X,Ls):-
	literalList(Ds,X,Ls).

% literalList(+Ds,+X,-Ls):- Ls literalok listaja mely elemei az
% X eleme D DL allitasnak felelnek meg, ahol D eleme Ds-nek
literalList([],_,[]).
literalList([D|Ds],X,[L|Ls]):-
	toLiteral(D,X,L),
	literalList(Ds,X,Ls).

% toLiteral(C,X,L):- C egy literal fogalom, L pedig a C(X) literal
toLiteral(top,_,true):- !.
toLiteral(bottom,_,not(true)):- !.
toLiteral(not(D),X,not(D2)):- !,
	toLiteral(D,X,D2).
toLiteral(C,X,concept(C,X)).
	 

% combineWithAnd(+As,+Ls,-Clauses):- az or([and(As)|Ls])
% formulanak megfelelo klozlista Clauses
% As klozok listaja
% Ls egyetlen kloz, azaz literalok listaja
combineWithAnd(Ands,Literals,Clauses):-
	findall(Clause, (
			  member(A,Ands),
			  append(A,Literals,Clause)
			), Clauses
	       ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% segedeljarasok az atleast fogalmak forditasahoz
createFunNames(0,FUN,FUN,[]):- !.
createFunNames(N,FUN,NFUN,[Name|NameList]):-
	nextName(FUN,Name),
	N1 is N - 1,
	createFunNames(N1,Name,NFUN,NameList).

% atleastClauses(+FunList,+Role,+Concept,+Variable,-AtleastClauseList)
atleastClauses([],_,_,_,[]).
atleastClauses([F|FunList],R,C,X,[[role(R,X,fun(F,X,_))],[FOLC]|AtleastClauseList]):-
	toLiteral(C,fun(F,X,_),FOLC),
	notEqualClauses(FunList,F,X,Equals),
	append(Equals,Rest,AtleastClauseList),
	atleastClauses(FunList,R,C,X,Rest).

%notEqualClauses(+FunList,+Fun,+Variable,-EqualClauseList)
notEqualClauses([],_,_,[]).
notEqualClauses([F1|FunList],Fun,X,[[not(role(eq,fun(F1,X,_),fun(Fun,X,_)))]|ClauseList]):-
	notEqualClauses(FunList,Fun,X,ClauseList).


% nextName(+Name,-NextName): NextName a lexikografikus rendezes
% szerinti Name utani betusorozat
nextName(N,NN):-
	atom_codes(N,Codes),
	reverse(Codes,R),
	nextList(R,NR),reverse(NR,NC),
	atom_codes(NN,NC).

nextList([C|Cs],[NC|NCs]):-
	(C < 122 ->
	    NC is C+1, NCs=Cs;
	    NC=97,nextList(Cs,NCs)).
nextList([],[97]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% segedeljarasok azt atmost fogalmak forditasahoz

% atmostLiterals(+Vars,+Role,+Concept,+Variable,-AtmostClause)
atmostLiterals([],_,_,_,[]).
atmostLiterals([V|Vars],R,C,X,[not(role(R,X,V)),FOLC|Literals]):-
	equalLiterals(V,Vars,Equals),
	append(Equals,Rest,Literals),
	neg(C,NC),
	toLiteral(NC,V,FOLC),
	atmostLiterals(Vars,R,C,X,Rest).

equalLiterals(_,[],[]):- !.
equalLiterals(V,[V2|Vars],[role(eq,V,V2)|Equals]):-
	equalLiterals(V,Vars,Equals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
