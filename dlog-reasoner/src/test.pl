:- use_module('dlog').
:- use_module('dl_interp').
:- set_prolog_flag(unknown, fail).

lubm(N,Q):-
	( N == 1 -> load_KB_interpreter('/home/stilgar/BME/Doktori/dlog/LUBM/lubm1.dig')
	; N == 2 -> load_KB_interpreter('/home/stilgar/BME/Doktori/dlog/LUBM/lubm2.dig')
	; N == 3 -> load_KB_interpreter('/home/stilgar/BME/Doktori/dlog/LUBM/lubm3.dig')
	),
	( Q == 1 -> solve('a:Chair')
	; Q == 2 -> solve_n((X,Y), ('a:Chair'(X),'a:worksFor'(X,Y),'a:Department'(Y), 'a:subOrganizationOf'(Y,'p25:www.University0.edu')))
	; Q == 3 -> solve_n((X,Y,Z), ('a:Student'(X),'a:Faculty'(Y),'a:Course'(Z), 'a:advisor'(X,Y), 'a:takesCourse'(X,Z), 'a:teacherOf'(Y,Z)))
	).

lubm_ford(N,Q):-
	abolish(abox:_),
	( N==1 -> dlkb2prolog('/home/stilgar/BME/Doktori/dlog/LUBM/lubm1.dig', './lubm1',[allinone(yes),statistics(yes)]), compile('./lubm1_tbox.pl')
	; N==2 -> dlkb2prolog('/home/stilgar/BME/Doktori/dlog/LUBM/lubm2.dig', './lubm2',[allinone(yes),statistics(yes)]), compile('./lubm2_tbox.pl')	    
	; N==3 -> dlkb2prolog('/home/stilgar/BME/Doktori/dlog/LUBM/lubm_3.dig', './lubm3',[allinone(yes),statistics(yes),filter_duplicates(yes)]), compile('./lubm3_tbox.pl')
	), !,
	( Q == 1 -> statistics_call('a:Chair'(_),B)
	; Q == 2 -> statistics_call(('a:subOrganizationOf'(Y,'p25:www.University0.edu'),'a:worksFor'(X,Y),'a:Chair'(X),'a:Department'(Y)),B)
	; Q == 3 -> statistics_call(('a:advisor'(X,Y), 'a:takesCourse'(X,Z), 'a:teacherOf'(Y,Z), 'a:Student'(X),'a:Faculty'(Y),'a:Course'(Z)),B)
	),
	print(B).


vicodi(Q):-
	load_KB_interpreter('/home/stilgar/BME/Doktori/dlog/vicodi/vicodi.dig'),
	( Q == 1 -> solve('Individual')
 	; Q == 2 -> solve_n((X,Y,Z),('Military-Person'(X), 'hasRole'(Y,X),'related'(X,Z)))
	).

vicodi_ford(Q):-
	abolish(abox:_),	
	dlkb2prolog('/home/stilgar/BME/Doktori/dlog/vicodi/vicodi.dig', './vicodi',[allinone(yes),statistics(yes),filter_duplicates(yes)]),
	compile('./vicodi_tbox.pl'), !,
	( Q == 1 -> statistics_call('Individual'(_),B)
 	; Q == 2 -> statistics_call(('hasRole'(_,X),'related'(X,_),'Military-Person'(X)),B)
	),
	print(B).

semintec(Q):-
	load_KB_interpreter('d:/Logic/ontology/semintec0.dig'),
	( Q = 1 -> solve_n((X,Y,Z),('Man'(X),'isCreditCardOf'(Y,X),'Gold'(Y),'livesIn'(X,Z),'Region'(Z)))
	; Q = 2 -> solve_n((X,Y,Z,W),('Woman'(X),'isOwnerOf'(X,Y),'Account'(Y),'hasLoan'(Y,Z),'hasPermanentOrder'(Y,W)))
	).

semintec_ford(Q):-
	abolish(abox:_),	
	dlkb2prolog('d:/Logic/ontology/semintec0.dig', './semintec0',[allinone(yes),statistics(yes)]),
	compile('./semintec0_tbox.pl'), !,
	( Q == 1 -> statistics_call(('isCreditCardOf'(Y,X),'livesIn'(X,Z),'Man'(X),'Gold'(Y),'Region'(Z)),B)
	; Q == 2 -> statistics_call(('isOwnerOf'(X,Y),'hasLoan'(Y,_),'hasPermanentOrder'(Y,_),'Woman'(X),'Account'(Y)),B)
	),
	print(B).
