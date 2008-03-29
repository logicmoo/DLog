:- module(show,[show/1]).

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

:- multifile user:portray/1.

user:portray(eq(X,Y)):-
	print(X),print('='),print(Y).
user:portray(not(A)):-
	print('-'),print(A).

user:portray(aconcept(C)):-
	print(C).
user:portray(nconcept(C)):-
	print(C).
user:portray(arole(R)):-
	print(R).
	
user:portray(aconcept(C,X)):-
	print(C),print('('),print(X),print(')').
user:portray(nconcept(C,X)):-
	print(C),print('('),print(X),print(')').
user:portray(arole(C,X,Y)):-
	print(C),print('('),print(X),print(','),print(Y),print(')').
user:portray(fun(FNAME,X)):-
	print(FNAME),print('('),print(X),print(')').
user:portray(marked(X)):-
	print('<'),print(X),print('>').

user:portray(and([C])):-
	print(C).
user:portray(and([C1,C2|Rest])):-
	print('('),
	portrayList([C1,C2|Rest],' & '),
	print(')').
user:portray(or([C])):-
	print(C).
user:portray(or([C1,C2|Rest])):-
	print('('),
	portrayList([C1,C2|Rest],' | '),
	print(')').


portrayList([C],_):-
	print(C).
portrayList([C1,C2|Rest],Op):-		
	print(C1),print(Op),
	portrayList([C2|Rest],Op).

