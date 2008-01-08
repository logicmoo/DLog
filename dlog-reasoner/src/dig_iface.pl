/***   Tudasbazis es lekerdezes beolvasasa DIG formatumu fajlbol

    Entry point:
      - read_tell(+DIG_Input,-TAxioms,-AAxioms)
      - read_ask(+DIG_Input,-TAxioms,-Queries)

  Keszitette: Borosan Peter (BME-SZIT)
              pborosan@cs.bme.hu
      
***/

:- module(dig_iface,[read_tell/3,read_asks/3, read_tell/4]).

:- use_module('../xml_reader/xml_parser').
:- use_module(library(lists)).
:- use_module('show').

counter(impliesc).
counter(equalc).
counter(related).
counter(instanceof).
counter(impliesr).
counter(equalr).
counter(functional).
counter(domain).
counter(range).
counter(differentfrom).
counter(alldifferent).
counter(disjoint).
counter(transitive).


% read_tell(+DIG_Input,-TAxioms,-AAxioms): DIG_Input tell DIG fajlbol
% TAxioms-ba es AAxioms-ba olvassa be a terminologiai- es adatallitasokat
read_tell(DIG_Input,TAxioms,AAxioms):-
	read_tell(DIG_Input, TAxioms, AAxioms, _).

read_tell(DIG_Input,TAxioms,AAxioms, Statistics):-
	reset_counters,
	parse_XML_DOM(DIG_Input,env(tells,_,EnvAxs)),
	bb_put(ac,1),
	find_functional(EnvAxs,FunRoles),
	sort(FunRoles,FunRoles2),
	envs_to_axioms(EnvAxs,FunRoles2,TAxioms,AAxioms),
	findall(CName-CValue, (counter(CName), bb_get(CName, CValue)), Statistics).


% envs_to_axioms(+Nodes,+FunRoles-TAxioms,-AAxioms): Nodes XML nodeok listaja,
% TAxioms es AAxioms az ezeknek megfelelo terminologiai- es adataxiomak
% FunRoles pedig a funkcionalis szerepek listaja
envs_to_axioms([env(impliesc,[EC1,EC2])|Es],FunRoles,[implies(C1,C2)|TAxioms],AAxioms):-
	env_to_concept(EC1,C1),env_to_concept(EC2,C2),	
	!,
	inc_counter(impliesc), % LG
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).

envs_to_axioms([env(equalc,[EC1,EC2])|Es],FunRoles,[equiv(C1,C2)|TAxioms],AAxioms):-
	env_to_concept(EC1,C1),env_to_concept(EC2,C2),
	!,
	inc_counter(equalc),	% LG
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).


envs_to_axioms([env(impliesr,[ER1,ER2])|Es],FunRoles,[subrole(R1,R2)|TAxioms],AAxioms):-
	env_to_role(ER1,R1), env_to_role(ER2,R2),
	!,
	inc_counter(impliesr),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).


envs_to_axioms([env(equalr,[ER1,ER2])|Es],FunRoles,[subrole(R1,R2),subrole(R2,R1)|TAxioms],AAxioms):-
	env_to_role(ER1,R1), env_to_role(ER2,R2),
	!,
	inc_counter(equalr),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).

/*	
envs_to_axioms([env(functional,[ER])|Es],FunRoles,[implies(top,atmost(1,R,top))|TAxioms],AAxioms):-
	env_to_role(ER,R),
	!,
	inc_counter(functional),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).
*/

envs_to_axioms([env(domain,[ER,EC])|Es],FunRoles,[implies(some(R,top),C)|TAxioms],AAxioms):-
	env_to_role(ER,R), env_to_concept(EC,C),
	!,
	inc_counter(domain),
	( select(R,FunRoles,RestFun) ->
	    TAxioms = [implies(C,some(R,top)),implies(C,atmost(1,R,top))|TAxioms2],
	    envs_to_axioms(Es,RestFun,TAxioms2,AAxioms)
	; envs_to_axioms(Es,FunRoles,TAxioms,AAxioms)
	).

envs_to_axioms([env(range,[ER,EC])|Es],FunRoles,[implies(top,all(R,C))|TAxioms],AAxioms):-
	env_to_role(ER,R), env_to_concept(EC,C),
	!,
	inc_counter(range),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).


envs_to_axioms([env(disjoint,[EC1,EC2])|Es],FunRoles,[implies(and([C1,C2]),bottom)|TAxioms],AAxioms):-
	env_to_concept(EC1,C1), env_to_concept(EC2,C2),
	!,
	inc_counter(disjoint),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).

envs_to_axioms([env(transitive,[ER])|Es],FunRoles,[trans(R)|TAxioms],AAxioms):-
	env_to_role(ER,R),
	!,
	inc_counter(transitive),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).


envs_to_axioms([env(related,[env(individual,[name=I1],[]),ER,env(individual,[name=I2],[])])|Es],FunRoles,TAxioms,[arole(R,I1,I2)|AAxioms]):-
	env_to_role(ER,arole(R)),
	!,
	inc_counter(related), % LG
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).

envs_to_axioms([env(instanceof,[env(individual,[name=I],[]),EC])|Es],FunRoles,TAxioms,[AAxiom|AAxioms]):-
	env_to_concept(EC,C),
	( C=aconcept(CN) -> AAxiom = aconcept(CN,I)
	; C=not(aconcept(CN)) -> AAxiom = not(aconcept(CN,I))
	),
	!,
	inc_counter(instanceof), % LG	
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).
/*
envs_to_axioms([env(differentfrom,[env(individual,[name=I1],[]),env(individual,[name=I2],[])])|Es],FunRoles,TAxioms,[not(eq(I1,I2))|AAxioms]):-
	!, inc_counter(differentfrom),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).

envs_to_axioms([env(alldifferent,Is)|Es],FunRoles,TAxioms,AAxioms):-
	!, inc_counter(alldifferent),
	findall(NE, (
		      member(Ins1,Is),
		      append(_,[Ins1|Rest],Is),
		      member(Ins2,Rest),
		      Ins1 = env(individual,[name=I1],[]),
		      Ins2 = env(individual,[name=I2],[]),
		      NE = not(eq(I1,I2))
		    ), AllDifferent
	       ),
	append(AllDifferent,AAxioms2,AAxioms),
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms2).
*/
envs_to_axioms([_|Es],FunRoles,TAxioms,AAxioms):-
	envs_to_axioms(Es,FunRoles,TAxioms,AAxioms).
envs_to_axioms([],FunRoles,TAxioms,[]):-
	findall(T, (
		     member(R,FunRoles),
		     ( T = implies(top,some(R,top))
		     ; T = implies(top,atmost(1,R,top))
		     )				  
		   ), TAxioms).



% env_to_role(+Node -Role)
% Role a Node XML nodenak megfelelo fogalom
env_to_role(env(ratom,[name=R],[]),arole(R)).
env_to_role(env(inverse,[env(ratom,[name=R],[])]),arole(InvRole)):-	
	atom_concat('inv_',R,InvRole).
env_to_role(env(inverse,[env(inverse,[Role])]),R):-
	env_to_role(Role,R).


% env_to_concept(+Node,-Concept): Concept a Node XML nodenak
% megfelelo fogalom
env_to_concept(env(top,[]),top).
env_to_concept(env(bottom,[]),bottom).
env_to_concept(env(catom,[name=A],[]),aconcept(A)).

env_to_concept(env(and,ECs),and(Cs)):-
	!,envs_to_concepts(ECs,Cs).
env_to_concept(env(or,ECs),or(Cs)):-
	!,envs_to_concepts(ECs,Cs).
env_to_concept(env(not,[EC]),not(C)):-
	!,env_to_concept(EC,C).
env_to_concept(env(some,[ER,EC]),some(R,C)):-
	!, env_to_role(ER,R), env_to_concept(EC,C).
env_to_concept(env(all,[ER,EC]),all(R,C)):-
	!, env_to_role(ER,R), env_to_concept(EC,C).

env_to_concept(env(atleast,[num=N],[ER,EC]),atleast(N1,R,C)):-
	!, env_to_role(ER,R), env_to_concept(EC,C), atom_codes(N,NC), number_codes(N1,NC).
env_to_concept(env(atmost,[num=N],[ER,EC]),atmost(N1,R,C)):-
	env_to_role(ER,R), env_to_concept(EC,C), atom_codes(N,NC), number_codes(N1,NC).



% envs_to_concepts(+Envs,-Concepts)
envs_to_concepts([EC|ECs],[C|Cs]):-
	env_to_concept(EC,C),
	envs_to_concepts(ECs,Cs).
envs_to_concepts([],[]).

% get_next_acnum(-NextNum)
get_next_acnum(N):-
	bb_get(ac,N),
	N1 is N+1,
	bb_put(ac,N1).

inc_counter(Counter) :-
	bb_get(Counter, C),
	C1 is C+1,
	bb_put(Counter, C1).

% read_ask(+DIG_Input,-TAxioms,-Queries): DIG_Input ask DIG fajlbol
% Queries-be beolvassa a lekerdezeseket, TAxioms pedig az uj fogalmakhoz
% szukseges terminologiai allitasok
read_asks(DIG_Input,TAxioms,Queries):-
	parse_XML_DOM(DIG_Input,env(asks,_,EnvAsks)),
	envs_to_asks(EnvAsks,TAxioms,Queries).

% envs_to_asks(+Envs,-TAxioms,-Queries)
envs_to_asks([env(instances,[EC])|Es],TAs,[Query-C|Queries]):-
	!,env_to_concept(EC,C),
	(
	  C=aconcept(CN) ->
	  Query=CN,
	  TAs=TAxioms
	;
	  get_next_acnum(ACNum),
	  number_chars(ACNum,ACCodes),atom_codes(ANum,ACCodes),
	  atom_concat('AC_',ANum,ACName),
	  Query=ACName,TAs=[implies(C,aconcept(ACName))|TAxioms]
	),
	envs_to_asks(Es,TAxioms,Queries).

envs_to_asks([],[],[]).

reset_counters :-
	counter(C),
	bb_put(C, 0),
	fail.
reset_counters.

% find_functional(+EnvAxs,-FunRoles)
% FunRoles azon szerepek listaja, melyekrol ki van jelentve,
% hogy funkcionalisak
find_functional([],[]).
find_functional([env(functional,[ER])|Es],[R|FunRoles]):-
	env_to_role(ER,R),
	find_functional(Es,FunRoles).
find_functional([_|Es],FunRoles):-
	find_functional(Es,FunRoles).