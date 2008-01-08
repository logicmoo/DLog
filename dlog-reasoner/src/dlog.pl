:- module(dlog, [dlkb2prolog/2, dlkb2prolog/3, load_KB/1, load_KB/2, load_KB_interpreter/1, load_KB0/6,write_tbox/2,statistics_call/2]).

:- use_module('struct').
:- use_module('dig_iface').
:- use_module('translator').
:- use_module('show').
:- use_module('dl_interp',[neg/2, assert_clauses/1, solve/1, solve_n/2]).
:- use_module(library(lists)).
:- use_module('prolog_translator',[dl2prolog/3]).

user:runtime_entry(start) :-
	start_dlog.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog fordítás
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% abolish(abox:_), dlkb2prolog('../examples/iocaste/c10.dig', '../output/c10', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/happy/happy.dig', '../output/happy', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/alcoholic/ca10.dig', '../output/ca10', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/shi/inverse.dig', '../output/inverse', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/shi_pelda.dig', '../output/shi_pelda', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/LUBM/lubm1.dig', '../output/lubm1', [allinone(yes), statistics(yes)]).
dlkb2prolog(KBFile, PFile, Options) :-
	load_KB0(KBFile, _Statistics, TBox, ABox, IBox, HBox),
	statistics(runtime, [T0,_]),
	dl2prolog(PFile, kb(TBox, ABox, IBox, HBox), Options),
	statistics(runtime, [T1,_]),Tt is T1-T0,
	format(' Prolog clauses have been generated in ~t~20|~t~3d sec ~n', [Tt]).

load_KB0(KBFile, Statistics, TBox_Clauses, ABox, IBox, HBox):-
	statistics(runtime, [T0,_]),
	read_tell(KBFile,TBox,ABox, Statistics),
	statistics(runtime, [T1,_]),T is T1-T0,
	format(' DIG KB read in ~t~20|~t~3d sec ~n', [T]),
	axioms_to_clauses(TBox,TBox_Clauses, IBox,HBox),
	statistics(runtime, [T2,_]),TA is T2-T1,
	format(' Axioms translated in ~t~20|~t~3d sec ~n', [TA]),	
	( member([],TBox_Clauses) -> nl, nl, write('Inconsistent TBox'), nl, nl, fail
	; true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog fordítás vége
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_tbox(KBFile, OutFile) :-
	load_KB(KBFile),
	open(OutFile, write, Stream),
	call_cleanup(flush_tbox(Stream), close(Stream)).

flush_tbox(Stream) :-
	current_predicate(_, dl:Head),
	clause(dl:Head, Body),
	Body \== true,
	portray_clause(Stream, (Head :- Body)),
	fail.
flush_tbox(_).

load_KB_interpreter(KBFile):-
	statistics(runtime, [T0,_]),
	read_tell(KBFile,TBox,ABox,_),
	statistics(runtime, [T1,_]),T is T1-T0,
	format(' DIG KB read in ~t~20|~t~3d sec ~n', [T]),
	axioms_to_clauses(TBox,TBox_Clauses),
	statistics(runtime, [T2,_]),TA is T2-T1,
	format(' Axioms translated in ~t~20|~t~3d sec ~n', [TA]),	
	( member([],TBox_Clauses) -> nl, nl, write('Inconsistent TBox'), nl, nl, fail
	; true
	),
	abolish(dl:_),
	clauses_to_prolog(TBox_Clauses,ABox),
	statistics(runtime, [T3,_]),TB is T3-T2,
	format(' Clauses asserted in ~t~20|~t~3d sec ~n', [TB]).
	
		    

% load_KB(+KBFile): KBFile DIG fajlban talalhato tudasbazis betoltese
load_KB(KBFile) :-
	load_KB(KBFile, _).

load_KB(KBFile, Statistics):-
	statistics(runtime, [T0,_]),
	read_tell(KBFile,TBox,ABox, Statistics),
	statistics(runtime, [T1,_]),T is T1-T0,
	format(' DIG KB read in ~t~20|~t~3d sec ~n', [T]),
	run(TBox, ABox).

% load_KB(+KBFile): KBFile DIG fajlban talalhato tudasbazis betoltese
load_KB(KBFile,TBox,ABox):-
	statistics(runtime, [T0,_]),
	read_tell(KBFile,TBox,ABox),
	statistics(runtime, [T1,_]),T is T1-T0,
	format(' DIG KB read in ~t~20|~t~3d sec ~n', [T]).

run(TBox,ABox):-
	statistics(runtime, [T0,_]),
	axioms_to_clauses(TBox,TBox_Clauses),
	statistics(runtime, [T1,_]),TA is T1-T0,
	format(' Axioms translated in ~t~20|~t~3d sec ~n', [TA]),	
	( member([],TBox_Clauses) -> nl, nl, write('Inconsistent TBox'), nl, nl, fail
	; true
	),
	abolish(dl:_),
	clauses_to_prolog(TBox_Clauses,ABox),
	statistics(runtime, [T2,_]),TB is T2-T1,
	format(' Clauses asserted in ~t~20|~t~3d sec ~n', [TB]).



clauses_to_prolog(TBox_Clauses,ABox):-
	findall(P, (
		     ( member(C,TBox_Clauses)
		     ; member(A,ABox), C=[A] % TODO fail-t ki kell szedni innen
		     ),
		     cls_to_neglist(C,P)
		   ), Neglist
	       ),
	assert_clauses(Neglist).


/***************************** Klozok atalakitasa *********************************/

% cls_to_neglist(+Cls,-NL): Cls klozt alakitja at a fordito modul formatumarol
% az interpreter altal ertelmezett formatumra
cls_to_neglist([not(L)|Ls],[FL|FLs]):-
	!,trans(L,FL),
	cls_to_neglist(Ls,FLs).
cls_to_neglist([L|Ls],[FL|FLs]):-
	!,trans(not(L),FL),
	cls_to_neglist(Ls,FLs).
cls_to_neglist([],[]).

% trans(+L,-H): H az L literal struktura alakban
trans(not(L),NTL):-
	!,
	trans(L,TL),
	neg(TL, NTL).
trans(eq(X,Y),eq(X,Y)):- !.
trans(L,C):-
	L=..[T,N|P],
	( T=nconcept-> atom_concat('NC_',N,NN),
	    C=..[NN|P]
	; C=..[N|P]
	).



/***************************************************/
/********************* peldak **********************/
/***************************************************/

sample1:-
	TBox = [	implies(top,atmost(1,arole(gyereke),top)),
			implies(top,some(arole(gyereke),aconcept(gazdag))),
			implies(top,some(arole(gyereke),aconcept(boldog))),
			implies(some(arole(gyereke),and([aconcept(gazdag),aconcept(boldog)])),aconcept(query))						
	       ],
	ABox = [],
	run(TBox,ABox),	
	solve(query).


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
	ABox = [],
	run(TBox,ABox).	

		
sample3:-
	TBox	= [
		   implies(top, atmost(2,arole(hasChild),top)),
		   implies(and([atmost(1,arole(hasChild), not(aconcept(rich))), some(arole(hasChild),aconcept(rich))]),aconcept(satisfied)),
		   implies(some(arole(inv_hasDescendant), aconcept(rich)),aconcept(rich)),
		   subrole(arole(hasChild),arole(hasDescendant)),
		   trans(arole(hasDescendant))		   
		  ],	
	ABox =	[
		 arole(hasChild,a,b),
		 arole(hasChild,a,c),
		 arole(hasChild,d,e),
		 arole(hasChild,e,c),
		 aconcept(rich,d)						
		],
	run(TBox,ABox),	
	solve(satisfied).
	
sample4:-
	TBox  = [
		 implies(top, atmost(2,arole(hasChild),top)),
		 implies(atmost(1,arole(hasChild), not(aconcept(rich))),aconcept(satisfied)),
		 subrole(arole(hasChild),arole(hasDescendant))
		],
	ABox =	[
		 arole(hasChild,a,b),						
		 aconcept(rich,b)
		],
	run(TBox,ABox),	
	solve(satisfied).
	
sample5:-
	TBox  = [
		 implies(some(arole(gyereke),and([aconcept(apagyilkos), some(arole(gyereke),not(aconcept(apagyilkos)))])), aconcept(iokaszte_szeru))
		],
	ABox =	[
		 arole(gyereke,i,o),
		 arole(gyereke,i,p1),
		 arole(gyereke,i,p2),
		 arole(gyereke,o,p2),
		 arole(gyereke,p2,p1),
		 arole(gyereke,p1,t),
		 
		 aconcept(apagyilkos,o),
		 not(aconcept(apagyilkos,t))
		],
	run(TBox,ABox),	
	solve(iokaszte_szeru).

sample6:-
	TBox  = [
		 implies(aconcept(n), atleast(2, arole(r),top)),
		 implies(atleast(2, arole(r),top), aconcept(t))
		],
	ABox =	[
		 aconcept(n,a)
		],
	run(TBox,ABox),	
	solve(t).
	
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
	ABox =	[
		 aconcept(gapc,pc1)
		],
	run(TBox,ABox),
	solve(correct).
	
sample8:-
	TBox  = [
		 implies(not(aconcept(rich)),aconcept(happy))
		],
	ABox =	[
		 not(aconcept(rich,a)),
		 eq(a,b),
		 eq(c,a),
		 eq(c,d)
		],
	run(TBox,ABox),	
	solve(happy).

sample9:-
	TBox = [
		implies(top,atmost(2,arole(gyereke),aconcept(ravasz))),		
		implies(top,atleast(2,arole(gyereke),aconcept(okos))),
		implies(top,atleast(2,arole(gyereke),aconcept(ravasz))),
		implies(some(arole(gyereke),aconcept(ravasz)),aconcept(szerencses))
	       ],
	ABox = [],		
	run(TBox,ABox).
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
	ABox = [
		not(aconcept(szep,rofi)),
		aconcept(buszke,hugo),
		aconcept(apa,ubul),
		aconcept(boldog,oid),
		not(aconcept(gazdag,thesz)),
		arole(gyereke,ubul,oid),
		arole(gyereke,ubul,polu),
		arole(gyereke,oid,polu),
		arole(gyereke,polu,thesz),
		arole(gyereke,rofi,koca),
		not(aconcept(no,koca)),
		arole(baratja,hugo,odon),
		arole(komoly,odon)
	       ],
	run(TBox,ABox).
sample11:-
	TBox = [
		implies(top,some(arole(gyereke),aconcept(okos))),		
		implies(top,atmost(1,arole(gyereke),top))
	       ],
	ABox = [],		
	run(TBox,ABox).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% runtime system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_conf :-
	current_predicate(_,conf:Head),
	retractall(conf:Head),
	fail.
clean_conf.

start_dlog :-
	log('DLog reasoner 0.1 alpha started~n~n',[]),
	load_config,
	conf:input(Input),
	conf:output(Output),
	conf:options(Options),
	log('Input ontology: ~w~n', [Input]),
	log('Ouput Prolog code: ~w~n~n', [Output]),
	log('Compilation options: ~w~n~n', [Options]),
	assert(env_default:alma(1)), % hack to supress a warning flag
	dlkb2prolog(Input, Output, Options),
	log('~nProlog code has been generated, exiting...', []).

load_config :-
	clean_conf, 
	open('../dlog.conf', read, Handle),
	load_config0(Handle),
	close(Handle),
	log('Configuration file has been successfully processed.~n~n',[]).

load_config0(Handle) :-
	catch(read(Handle, Term), Error, error_handler(Error)),
	(
	  Term \= end_of_file ->
	  assert(conf:Term),
	  load_config0(Handle)
	;
	  true
	).

% log(+Message, +Arguments)
log(Message, Arguments) :-
	format(Message, Arguments).

error_handler(syntax_error(_Goal, Position, _Message, _Tokens, _AfterError)) :-
	log("Error: Cannot read, problem with line: ~w~n",[Position]),
	fail.