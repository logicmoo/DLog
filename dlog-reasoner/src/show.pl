:- module(show,[show/1, portray/1]).

/***************************************************/
/****************** megjelenites *******************/
/***************************************************/

show(X):-	
	show(X,0).
show([],_).
show([A|Rest],N):-
	N1 is N + 1,
	print(N1),print(': '),print(A),nl,	
	show(Rest,N1).


portray(eq(X,Y)):-
	print(X),print('='),print(Y).
portray(not(A)):-
	print('-'),print(A).

portray(aconcept(C)):-
	print(C).
portray(nconcept(C)):-
	print(C).
portray(arole(R)):-
	print(R).
	
portray(aconcept(C,X)):-
	print(C),print('('),print(X),print(')').
portray(nconcept(C,X)):-
	print(C),print('('),print(X),print(')').
portray(arole(C,X,Y)):-
	print(C),print('('),print(X),print(','),print(Y),print(')').
portray(fun(FNAME,X)):-
	print(FNAME),print('('),print(X),print(')').
portray(marked(X)):-
	print('<'),print(X),print('>').

portray(and([C])):-
	print(C).
portray(and([C1,C2|Rest])):-
	print('('),
	portrayList([C1,C2|Rest],' & '),
	print(')').
portray(or([C])):-
	print(C).
portray(or([C1,C2|Rest])):-
	print('('),
	portrayList([C1,C2|Rest],' | '),
	print(')').
	
portrayList([C],_):-
	print(C).
portrayList([C1,C2|Rest],Op):-		
	print(C1),print(Op),
	portrayList([C2|Rest],Op).