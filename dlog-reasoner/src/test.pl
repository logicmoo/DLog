:- use_module('dlog').
:- use_module('dl_interp').
:- use_module('show').
:- use_module('translator').
:- set_prolog_flag(unknown, fail).

translate(TBox):-
	statistics(runtime, [T0,_]),
	axioms_to_clauses(TBox,TBox_Clauses,IBox,HBox,Transitives),
	statistics(runtime, [T1,_]),TA is T1-T0,
	format(' Axioms translated in ~t~20|~t~3d sec ~n', [TA]),
	( member([],TBox_Clauses) -> nl, nl, write('Inconsistent TBox'), nl, nl, fail
	; true
	),
	nl, print('Eredeti TBox'), nl, show(TBox),
	nl, print('Eredo TBox'), nl, show(TBox_Clauses), show(IBox), show(HBox), show(Transitives).


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

wine:-
	load_KB_interpreter('d:/Logic/ontology/wine2.dig'),
	solve('AmericanWine').


/***************************************************/
/********************* peldak **********************/
/***************************************************/

sample1:-
	TBox = [	implies(top,atmost(1,arole(gyereke),top)),
			implies(top,some(arole(gyereke),aconcept(gazdag))),
			implies(top,some(arole(gyereke),aconcept(boldog))),
			implies(some(arole(gyereke),and([aconcept(gazdag),aconcept(boldog)])),aconcept(query))						
	       ],
	translate(TBox).

sample2:-
	TBox = [	subrole(arole(rr),arole(tt)),
			subrole(arole(ss),arole(tt)),
			implies(aconcept(cc),some(arole(rr),top)),
			implies(top,some(arole(inv_ss),top)),						
			implies(top,atmost(1,arole(tt),top)),												
			implies(some(arole(ss),top),aconcept(dd)),
			implies(some(arole(rr),top),not(aconcept(dd))),
			implies(top,aconcept(cc))
	       ],
	translate(TBox).

sample22:-
	TBox = [
		implies(top,some(arole(gyereke),aconcept(okos))),
		implies(top,some(arole(inv_gyereke),aconcept(szep))),
		implies(top,atmost(1,arole(gyereke),aconcept(magas))),
		implies(top,atmost(1,arole(gyereke),aconcept(kover))),
		implies(top,or([aconcept(magas),aconcept(kover)]))
	       ],
	translate(TBox).

		
sample3:-
	TBox	= [
		   implies(top, atmost(2,arole(hasChild),top)),
		   implies(and([atmost(1,arole(hasChild), not(aconcept(rich))), some(arole(hasChild),aconcept(rich))]),aconcept(satisfied)),
		   implies(some(arole(inv_hasDescendant), aconcept(rich)),aconcept(rich)),
		   subrole(arole(hasChild),arole(hasDescendant)),
		   trans(arole(hasDescendant))		   
		  ],	
	translate(TBox).
	
sample4:-
	TBox  = [
		 implies(top, atmost(2,arole(hasChild),top)),
		 implies(atmost(1,arole(hasChild), not(aconcept(rich))),aconcept(satisfied)),
		 subrole(arole(hasChild),arole(hasDescendant))
		],
	translate(TBox).
	
sample5:-
	TBox  = [
		 implies(some(arole(gyereke),and([aconcept(apagyilkos), some(arole(gyereke),not(aconcept(apagyilkos)))])), aconcept(iokaszte_szeru))
		],
	translate(TBox).

sample6:-
	TBox  = [
		 implies(aconcept(n), atleast(2, arole(r),top)),
		 implies(atleast(2, arole(r),top), aconcept(t))
		],
	translate(TBox).
	
sample7:-
	TBox  = [
		 implies(top,all(arole(hasAdpt),aconcept(adpt))),
		 implies(top,all(arole(hasAcc3D),aconcept(acc3D))),
		 implies(and([aconcept(adpt),aconcept(acc3D)]), aconcept(adptAcc3D)),
		 implies(aconcept(adpt),aconcept(vd)),
		 implies(aconcept(acc3D),aconcept(vd)),
		 implies(aconcept(grws),aconcept(pc)),
		 implies(aconcept(gapc),and([aconcept(grws), aconcept(pc)])),
		 
		 subrole(arole(hasAdpt),arole(hasVD)),
		 subrole(arole(hasAcc3D),arole(hasVD)),
		 subrole(arole(hasVD),arole(contains)),
		 
		 trans(arole(contains)),
		 
		 implies(aconcept(pc),some(arole(hasAdpt),top)),
		 implies(aconcept(grws),some(arole(hasAcc3D),top)),

		 implies(aconcept(gapc),atmost(1,arole(hasVD),aconcept(vd))),
		 implies(aconcept(pci),all(arole(contains),aconcept(pci))),
		 
		 implies(some(arole(hasVD),aconcept(adptAcc3D)),aconcept(correct))
		],
	translate(TBox).
	
sample8:-
	TBox  = [
		 implies(not(aconcept(rich)),aconcept(happy))
		],
	translate(TBox).

sample9:-
	TBox = [
		implies(top,atmost(2,arole(gyereke),aconcept(ravasz))),		
		implies(top,atleast(2,arole(gyereke),aconcept(okos))),
		implies(top,atleast(2,arole(gyereke),aconcept(ravasz))),
		implies(some(arole(gyereke),aconcept(ravasz)),aconcept(szerencses))
	       ],
	translate(TBox).

sample10:-
	TBox = [
		equiv(aconcept(anya),and([aconcept(ember),aconcept(nonemu),some(arole(gyereke),top)])),
		equiv(and([aconcept(ember),aconcept(nonemu)]),aconcept(no)),		
		equiv(and([aconcept(ember),not(aconcept(nonemu))]),aconcept(ferfi)),
		equiv(and([aconcept(ember),some(arole(gyereke),top)]),aconcept(szulo)),		
		equiv(and([aconcept(ferfi),aconcept(szulo)]),aconcept(apa))
		
		% implies(and([aconcept(sikeres),not(aconcept(boldog))]),aconcept(gazdag)),
		% implies(aconcept(gazdag),aconcept(sikeres)),
		% implies(aconcept(boldog),aconcept(sikeres)),
		
		% implies(and([some(arole(inv_testvere),top),not(aconcept(no))]),aconcept(ferfi)),
		% implies(all(arole(gyereke),aconcept(boldog)),aconcept(joszulo)),
		% implies(some(arole(gyereke),top),aconcept(szep)),
		% implies(aconcept(buszke),some(arole(testvere),aconcept(gazdag)))
	       ],
	translate(TBox).

sample11:-
	TBox = [
		implies(top,some(arole(gyereke),aconcept(okos))),		
		implies(top,atmost(1,arole(gyereke),top))
	       ],
	translate(TBox).
