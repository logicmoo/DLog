:- use_module('../dlog').
:- use_module('../dl_translator/show').
:- use_module('../dl_translator/axioms_to_clauses',[axioms_to_clauses/6]).
:- use_module('../config',[set_dlog_option/2]).
% :- set_prolog_flag(unknown, fail).

compare(X):-
	atom_concat(sample,X,Y),
	set_dlog_option(dl_calculus,yes),
	nl, print('Uj szamitas: '), nl,
	call(Y),	
	set_dlog_option(dl_calculus,no),
	nl, print('Regi szamitas: '), nl,	
	call(Y).
	

test(TBox):-
	statistics(runtime, [T0,_]),
	axioms_to_clauses(_,TBox,TBox_Clauses,_,_,_),
	statistics(runtime, [T1,_]),TA is T1-T0,
	format(' Axioms tested in ~t~20|~t~3d sec ~n', [TA]),
	( member([],TBox_Clauses) -> nl, nl, write('Inconsistent TBox'), nl, nl
	; true
	),
	nl, print('Eredeti TBox'), nl, show(TBox),	
	nl, print('Eredo TBox'), nl, show(TBox_Clauses),
	true.


lubm(N):-
	set_dlog_option(dl_calculus,yes),
	set_dlog_option(dig_reader_fault_tolerance,drop),
	( N == 1 -> tell_dig('d:/Logic/ontology/lubm1.dig')
	; N == 2 -> tell_dig('d:/Logic/ontology/lubm2.dig')
	; N == 3 -> tell_dig('d:/Logic/ontology/lubm3.dig')
	).
/*
	( Q == 1 -> solve('a:Chair')
	; Q == 2 -> solve_n((X,Y), ('a:Chair'(X),'a:worksFor'(X,Y),'a:Department'(Y), 'a:subOrganizationOf'(Y,'p25:www.University0.edu')))
	; Q == 3 -> solve_n((X,Y,Z), ('a:Student'(X),'a:Faculty'(Y),'a:Course'(Z), 'a:advisor'(X,Y), 'a:takesCourse'(X,Z), 'a:teacherOf'(Y,Z)))
	).
*/
lubm_ford(N,Q):-
	abolish(abox:_),
	( N==1 -> dlkb2prolog('d:/Logic/ontology/lubm1.dig', './lubm1',[allinone(yes),statistics(yes)]), compile('./lubm1_tbox.pl')
	; N==2 -> dlkb2prolog('d:/Logic/ontology/lubm2.dig', './lubm2',[allinone(yes),statistics(yes)]), compile('./lubm2_tbox.pl')	    
	; N==3 -> dlkb2prolog('d:/Logic/ontology/lubm3.dig', './lubm3',[allinone(yes),statistics(yes),filter_duplicates(yes)]), compile('./lubm3_tbox.pl')
	), !,
	( Q == 1 -> statistics_call('a:Chair'(_),B)
	; Q == 2 -> statistics_call(('a:subOrganizationOf'(Y,'p25:www.University0.edu'),'a:worksFor'(X,Y),'a:Chair'(X),'a:Department'(Y)),B)
	; Q == 3 -> statistics_call(('a:advisor'(X,Y), 'a:takesCourse'(X,Z), 'a:teacherOf'(Y,Z), 'a:Student'(X),'a:Faculty'(Y),'a:Course'(Z)),B)
	),
	print(B).


vicodi(Q):-
	load_KB_interpreter('d:/Logic/ontology/vicodi.dig'),
	( Q == 1 -> solve('Individual')
 	; Q == 2 -> solve_n((X,Y,Z),('Military-Person'(X), 'hasRole'(Y,X),'related'(X,Z)))
	).

vicodi_ford(Q):-
	abolish(abox:_),	
	dlkb2prolog('d:/Logic/ontology/vicodi.dig', './vicodi',[allinone(yes),statistics(yes),filter_duplicates(yes)]),
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
	load_KB_interpreter('d:/Logic/ontology/wine_0.dig'),
	solve('AmericanWine').

wine_ford:-
	abolish(abox:_),	
	dlkb2prolog('d:/Logic/ontology/wine_0.dig', './wine',[allinone(yes),statistics(yes)]),
	compile('./wine_tbox.pl'), !,
	statistics_call(('AmericanWine'(_)),B),
	print(B).

/***************************************************/
/********************* peldak **********************/
/***************************************************/

sample1:-
	CInclusion = [
		      implies(top,atmost(1,arole(gyereke),top)),
		      implies(top,some(arole(gyereke),aconcept(gazdag))),
		      implies(top,some(arole(gyereke),aconcept(boldog))),
		      implies(some(arole(gyereke),and([aconcept(gazdag),aconcept(boldog)])),aconcept(query))						
		     ],
	test([CInclusion,[],[]]).

sample2:-
	CInclusion = [
		      implies(aconcept(cc),some(arole(rr),top)),
		      implies(top,some(inv(arole(ss)),top)),						
		      implies(top,atmost(1,arole(tt),top)),												
		      implies(some(arole(ss),top),aconcept(dd)),
		      implies(some(arole(rr),top),not(aconcept(dd))),
		      implies(top,aconcept(cc))
		     ],
	RInclusion = [
		      subrole(arole(rr),arole(tt)),
		      subrole(arole(ss),arole(tt))
		     ],			
	test([CInclusion,RInclusion,[]]).

sample3:-
	CInclusion = [
		      implies(top,some(arole(gyereke),aconcept(okos))),
		      implies(top,some(inv(arole(gyereke)),aconcept(szep))),
		      implies(top,atmost(1,arole(gyereke),aconcept(magas))),
		      implies(top,atmost(1,arole(gyereke),aconcept(kover))),
		      implies(top,or([aconcept(magas),aconcept(kover)]))
		     ],
	test([CInclusion, [],[]]).
		
sample4:-
	CInclusion = [
		      implies(top, atmost(2,arole(hasChild),top)),
		      implies(and([atmost(1,arole(hasChild), not(aconcept(rich))), some(arole(hasChild),aconcept(rich))]),aconcept(satisfied)),
		      implies(some(inv(arole(hasDescendant)), aconcept(rich)),aconcept(rich))
		      ],
	RInclusion = [
		      subrole(arole(hasChild),arole(hasDescendant))
		     ],
	Transitive = [
		      arole(hasDescendant)
		     ],
	test([CInclusion, RInclusion, Transitive]).

sample5:-
	CInclusion = [
		      implies(top, atmost(2,arole(hasChild),top)),
		      implies(atmost(1,arole(hasChild), not(aconcept(rich))),aconcept(satisfied))		      
		     ],
	RInclusion = [
		      subrole(arole(hasChild),arole(hasDescendant))
		     ],
	Transitive = [],
	test([CInclusion, RInclusion, Transitive]).

sample6:-
	CInclusion = [
		      implies(top, atleast(2, arole(hasDescencent),not(aconcept(rich)))),
		      implies(top,atmost(0,arole(hasChild),aconcept(rich)))
		     ],
	RInclusion = [
		      subrole(arole(hasChild),arole(hasDescendant))
		     ],
	Transitive = [],
	test([CInclusion, RInclusion, Transitive]).
	
sample7:-
	CInclusion  = [
		       implies(some(arole(gyereke),and([aconcept(apagyilkos), some(arole(gyereke),not(aconcept(apagyilkos)))])), aconcept(iokaszte_szeru))
		      ],
	test([CInclusion,[],[]]).

sample8:-
	CInclusion  = [
		       implies(aconcept(n), atleast(2, arole(r),top)),
		       implies(atleast(2, arole(r),top), aconcept(t))
		      ],
	test([CInclusion,[],[]]).
	
sample9:-
	CInclusion  = [
		       implies(top,all(arole(hasAdpt),aconcept(adpt))),
		       implies(top,all(arole(hasAcc3D),aconcept(acc3D))),
		       implies(and([aconcept(adpt),aconcept(acc3D)]), aconcept(adptAcc3D)),
		       implies(aconcept(adpt),aconcept(vd)),
		       implies(aconcept(acc3D),aconcept(vd)),
		       implies(aconcept(grws),aconcept(pc)),
		       implies(aconcept(gapc),and([aconcept(grws), aconcept(pc)])),
		       implies(aconcept(pc),some(arole(hasAdpt),top)),
		       implies(aconcept(grws),some(arole(hasAcc3D),top)),
		       
		       implies(aconcept(gapc),atmost(1,arole(hasVD),aconcept(vd))),
		       implies(aconcept(pci),all(arole(contains),aconcept(pci))),
		       
		       implies(some(arole(hasVD),aconcept(adptAcc3D)),aconcept(correct))
		      ],
	RInclusion = [
		      subrole(arole(hasAdpt),arole(hasVD)),
		      subrole(arole(hasAcc3D),arole(hasVD)),
		      subrole(arole(hasVD),arole(contains))
		     ],
	Transitive = [
		      arole(contains)		 
		     ],
	test([CInclusion,RInclusion,Transitive]).

sample10:-
	CInclusion = [
		      implies(top,atmost(2,arole(gyereke),aconcept(ravasz))),		
		      implies(top,atleast(2,arole(gyereke),aconcept(okos))),
		      implies(top,atleast(2,arole(gyereke),aconcept(ravasz))),
		      implies(some(arole(gyereke),aconcept(ravasz)),aconcept(szerencses))
		     ],
	test([CInclusion,[],[]]).

sample11:-
	CInclusion = [
		      implies(top,some(arole(gyereke),aconcept(okos))),		
		      implies(top,atmost(1,arole(gyereke),top))
		     ],
	test([CInclusion,[],[]]).

sample12:-
	CInclusion = [
		      implies(top, atleast(2,arole(gyereke),aconcept(okos))),
		      implies(top, atleast(2,arole(gyereke),aconcept(szep))),
		      implies(top, atleast(1,arole(gyereke),and([aconcept(okos),not(aconcept(szep))]))),
		      implies(top, atmost(3, arole(gyereke),top)),
		      implies(top, atmost(2, arole(gyereke),aconcept(okos)))
		     ],
	RInclusion = [],
	Transitive = [],
	test([CInclusion, RInclusion, Transitive]).


sample13:-
	CInclusion = [
		      implies(top, atleast(2,arole(gyereke),aconcept(okos))),
		      implies(top, atleast(3,arole(gyereke),or([not(aconcept(okos)),not(aconcept(ravasz))]))),
		      implies(top, atleast(2,arole(gyereke),aconcept(ravasz))),		      
		      implies(top, atmost(3, arole(gyereke),top))
		     ],
	RInclusion = [],
	Transitive = [],
	test([CInclusion, RInclusion, Transitive]).

sample14:-
	CInclusion = [
		      implies(top, atleast(2,arole(gyereke),aconcept(a))),
		      implies(top, atleast(2,arole(gyereke),aconcept(b))),		      
		      implies(top, atleast(2,arole(gyereke),aconcept(c))),
		      implies(top, atleast(2,arole(gyereke),aconcept(d))),

		      implies(and([aconcept(a),aconcept(b)]),not(aconcept(c))),
		      implies(and([aconcept(a),aconcept(b)]),not(aconcept(d))),
		      implies(and([aconcept(a),aconcept(c)]),not(aconcept(d))),
		      implies(and([aconcept(b),aconcept(c)]),not(aconcept(d))),
		      
		      %implies(top, atleast(1,aconcept(gyereke),or([not(aconcept(a)),not(aconcept(b)),not(aconcept(c))]))),
		      implies(top, atmost(3, arole(gyereke),top))
		     ],
	test([CInclusion, [],[]]).

sample15:-
	CInclusion = [
		      implies(top, atleast(2,arole(gyereke),aconcept(a))),
		      implies(top, atleast(2,arole(gyereke),aconcept(b))),		      
		      implies(top, atleast(2,arole(gyereke),aconcept(c))),

		      implies(and([aconcept(a),aconcept(b)]),not(aconcept(c))),
		      implies(top, atmost(3, arole(gyereke),top))
		     ],
	test([CInclusion, [],[]]).

sample16:-
	CInclusion = [
		      implies(top, atleast(2,arole(gyereke),aconcept(a))),
		      implies(top, atleast(2,arole(gyereke),aconcept(b))),		      
		      implies(top, atleast(2,arole(gyereke),aconcept(c))),
		      implies(top, atleast(2,arole(gyereke),aconcept(d))),
		      implies(top, atleast(2,arole(gyereke),aconcept(e))),

		      implies(and([aconcept(a),aconcept(b),aconcept(c)]),not(aconcept(d))),
		      implies(and([aconcept(a),aconcept(b),aconcept(c)]),not(aconcept(e))),
		      implies(and([aconcept(a),aconcept(b),aconcept(d)]),not(aconcept(e))),
		      implies(and([aconcept(a),aconcept(c),aconcept(d)]),not(aconcept(e))),
		      implies(and([aconcept(b),aconcept(c),aconcept(d)]),not(aconcept(e))),


		      implies(top, atmost(3, arole(gyereke),top))
		     ],
	test([CInclusion, [],[]]).




/*
sample11:-
	Tbox = [
		equiv(aconcept(anya),and([aconcept(ember),aconcept(nonemu),some(arole(gyereke),top)])),
		equiv(and([aconcept(ember),aconcept(nonemu)]),aconcept(no)),		
		equiv(and([aconcept(ember),not(aconcept(nonemu))]),aconcept(ferfi)),
		equiv(and([aconcept(ember),some(arole(gyereke),top)]),aconcept(szulo)),		
		equiv(and([aconcept(ferfi),aconcept(szulo)]),aconcept(apa))
		
		% implies(and([aconcept(sikeres),not(aconcept(boldog))]),aconcept(gazdag)),
		% implies(aconcept(gazdag),aconcept(sikeres)),
		% implies(aconcept(boldog),aconcept(sikeres)),
		
		% implies(and([some(inv(arole(testvere)),top),not(aconcept(no))]),aconcept(ferfi)),
		% implies(all(arole(gyereke),aconcept(boldog)),aconcept(joszulo)),
		% implies(some(arole(gyereke),top),aconcept(szep)),
		% implies(aconcept(buszke),some(arole(testvere),aconcept(gazdag)))
	       ],
	test(TBox).

*/