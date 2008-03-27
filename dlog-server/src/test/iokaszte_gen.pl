% v. 1.0
:- use_module(library(lists)).
:- use_module(library(random)).

% run_generator(+File)
% run_generator('../test/iokaszte.gen').
run_generator(File) :-
	abolish(env:_),
	read_suite(File),
	generate.

read_suite(File) :-
	open(File, read, Stream),
	call_cleanup(read_suite0(Stream), close(Stream)).

read_suite0(Stream) :-
	repeat,
	read_term(Stream, Term, []),
	(
	  Term == end_of_file -> !
	;
	  assert(env:Term),
	  fail
	).

generate :-
	env:testcase(N, Sz, M, D, File, Formats),
	once(graf(N, Sz, M, D, Graph)),
	export_results(Graph, File, Formats),
	fail.
generate.

export_results(Graph, File0, Formats) :-
	member(Format, Formats),
	atom_concat(File0, '.', File1),
	atom_concat(File1, Format, File),
	(
	  Format == dig ->
	  graf2dig(Graph, File)
	;
	  Format == racer ->
	  graf2racer(Graph, File)
	;
	  Format == owl ->
	  graf2owl(Graph, File)
	),
	format('File ~p in ~p format has been created~n',[File, Format]),
	fail.
export_results(_, _, _).

% graf(+N, +Sz, +M, +D, -G)
% G cimkezett, iranyitott graf tartalmaz egy 
% iokaszte szeru N meretu mintat
% A graf pontjainak szama N*Sz, az elek es csomopontok
% cimkei egy M elemu halmazbol kerulnek ki
% A graf elagazasi tenyezoje D
% A gráf egy par, amely tagjai
% el(?cimke, csp(+id), csp(+id)), illetve csp(+id)-?cimke
% alaku strukturak listaja
graf(N, Sz, M, D, g(Elek, Csucsok)) :-
	T is N*Sz,
	struktura(N, T, D, g(Elek, Csucsok), i(Pek, PPek, Pcsk), e(Vek, Eek, Csk)),
	elcimke_konstans(Pek), % piros elek szinezese
	elcimke(PPek, 1, M), % piros csucsok kozott futok szinezese
	elcimke(Eek, 0, M), % elore elek szinezese
	elcimke(Vek, 0, M), % visszaelek szinezese
	csucscimke(Pcsk, Elek, 1, M), % piros csucsok cimkezese
	csucscimke(Csk, Elek, 0, M). % minden mas csucs szinezese

struktura(N, T, D, G, I, E) :-
	    struktura(1, N, T, D, G, I, E).

struktura(1, N, T, D, g(Elek, Csucsok), i(Pek, PPek, Pcsk), e(Vek, Eek, Csk)) :-
	!,
	novekvo_lista(2, N, L0),
	vegpontok(N, T, D, V0), % N mar benne lehet
	append(L0, V0, V), % azon pontok listaja, ahova vezet el a gyokerbol
	elek(1, N, V, Elek0, Pek0, PPek0, Vek0, Eek0),
	append(Elek0, Elek1, Elek), append(Pek0, Pek1, Pek),
	append(PPek0, PPek1, PPek), append(Vek0, Vek1, Vek),
	append(Eek0, Eek1, Eek),
	Csucsok = [csp(1)-Cimke|Csucsok1],
	Pcsk = [csp(1)-Cimke|Pcsk1],
	struktura(2, N, T, D, g(Elek1, Csucsok1), i(Pek1, PPek1, Pcsk1), e(Vek1, Eek1, Csk)).
struktura(C, N, T, D, g(Elek, Csucsok), i(Pek, PPek, Pcsk), e(Vek, Eek, Csk)) :-
	C < N, !,
	C0 is C+1,
	vegpontok(1, T, D, V0), % barhova mehet el
	(
	  memberchk(C0, V0)->
	  V = V0
	;
	  append(V0, [C0], V)	% C0 kotelezoen benne van
	),
	elek(C, N, V, Elek0, Pek0, PPek0, Vek0, Eek0),
	append(Elek0, Elek1, Elek), append(Pek0, Pek1, Pek),
	append(PPek0, PPek1, PPek), append(Vek0, Vek1, Vek),
	append(Eek0, Eek1, Eek),
	Csucsok = [csp(C)-Cimke|Csucsok1],
	(
	  C = 2 ->
	  Cimke = a,
	  Pcsk1 = Pcsk
	;
	  Pcsk = [csp(C)-Cimke|Pcsk1]
	),
	struktura(C0, N, T, D, g(Elek1, Csucsok1), i(Pek1, PPek1, Pcsk1), e(Vek1, Eek1, Csk)).
struktura(C, N, T, D, g(Elek, Csucsok), i(Pek, PPek, Pcsk), e(Vek, Eek, Csk)) :-
	C = N, !,
	C0 is C+1,
	vegpontok(1, T, D, V), % barhova mehet el
	elek(C, N, V, Elek0, Pek0, PPek0, Vek0, Eek0),
	append(Elek0, Elek1, Elek), append(Pek0, Pek1, Pek),
	append(PPek0, PPek1, PPek), append(Vek0, Vek1, Vek),
	append(Eek0, Eek1, Eek),
	Csucsok = [csp(C)-not(a)|Csucsok1],
	struktura(C0, N, T, D, g(Elek1, Csucsok1), i(Pek1, PPek1, Pcsk), e(Vek1, Eek1, Csk)).
struktura(C, N, T, D, g(Elek, Csucsok), i(Pek, PPek, Pcsk), e(Vek, Eek, Csk)) :-
	C =< T, !,
	C0 is C+1,
	vegpontok(1, T, D, V),	% barhova mehet el
	elek(C, N, V, Elek0, Pek0, PPek0, Vek0, Eek0),
	append(Elek0, Elek1, Elek), append(Pek0, Pek1, Pek),
	append(PPek0, PPek1, PPek), append(Vek0, Vek1, Vek),
	append(Eek0, Eek1, Eek),
	Csucsok = [csp(C)-Cimke|Csucsok1],
	Csk = [csp(C)-Cimke|Csk1],
	struktura(C0, N, T, D, g(Elek1, Csucsok1), i(Pek1, PPek1, Pcsk), e(Vek1, Eek1, Csk1)).
struktura(_, _, _, _, g([], []), i([], [], []), e([], [], [])).
	
% vegpontok(K, T, D, V)
% V egy legfeljebb D elemu szamlista
% A szamok K es T kozott lehetnek
vegpontok(K, T, D, V) :-
	D0 is D+1,
	random(0,D0,D1), % mennyi el megy ki tenylegesen
	T1 is T+1,
	A is T1-K,
	D2 is min(A, D1),
	veletlen_sor(D2, K, T1, [], V).

veletlen_sor(0, _, _, Gy, V) :-
	!, V = Gy.
veletlen_sor(N, K, T, Gy, V) :-
	N > 0,
	random(K, T, V0),
	(
	  memberchk(V0, Gy)->
	  veletlen_sor(N, K, T, Gy, V)
	;
	  N1 is N-1,
	  veletlen_sor(N1, K, T, [V0|Gy], V)
	).

% elek(+K, +N, +L, -Elek, -Pek, -PPek, -Vek, -Eek)
% K kezdopontbol megy az L lista osszes pontjaba el
% Pek a piros elek listaja
% PPek a piros pontok kozott futo elek listaja
% Vek a visszaelek listaja
% Eek az elore elek listaja
% N az iokaszte minta pontjainak a szama
elek(K, N, L, Elek, Pek, PPek, Vek, Eek) :-
	elek0(L, K, N, Elek, Pek, PPek, Vek, Eek).

elek0([], _, _, [], [], [], [], []).
elek0([P|Ps], K, N, Elek, Pek, PPek, Vek, Eek) :-
	Elek = [el(Cimke,csp(K),csp(P))|Elek1],
	(
	  K = 1, P < N ->       % gyokerbol kiindulo elek
	  Pek = [Cimke|Pek1],
	  PPek1 = PPek,
	  Vek1 = Vek,
	  Eek1 = Eek
	;
	  K = 1, P = N ->       % gyokerbol vegpontba meno el
	  Pek1 = Pek,
	  PPek = [Cimke|PPek1],
	  Vek1 = Vek,
	  Eek1 = Eek
	;
	  K < N, P =:= K+1 ->
	  Pek = [Cimke|Pek1],
	  PPek1 = PPek,
	  Vek1 = Vek,
	  Eek1 = Eek
	;
	  K < N, P =< N ->
	  Pek1 = Pek,
	  PPek = [Cimke|PPek1],
	  Vek1 = Vek,
	  Eek1 = Eek
	;
	  K < P -> % eloreel
	  Pek1 = Pek,
	  PPek1 = PPek,
	  Vek1 = Vek,
	  Eek = [Cimke|Eek1]
	;
	  % visszael, hurokel
	  Pek1 = Pek,
	  PPek1 = PPek,
	  Eek1 = Eek,
	  Vek = [Cimke|Vek1]
	),
	elek0(Ps, K, N, Elek1, Pek1, PPek1, Vek1, Eek1).
	

% elcimke_konstans(?L)
elcimke_konstans([]).
elcimke_konstans([C|Cs]):-
	C = a,
	elcimke_konstans(Cs).

% elcimke(?L, +K, +V)
% L-ben levo elcimkek kitoltese
% Kezdoszin K, vegszin V
elcimke([], _, _).
elcimke([E|Es], K, V) :-
	random(K, V, R), !,
	C is R+97,
	atom_codes(E,[C]),
	elcimke(Es, K, V).
elcimke(_, _, _).

%csucscimke(?L, +K, +V)
csucscimke([], _, _, _).
csucscimke([csp(N)-C|Cs], Elek, K, V) :-
	random(0, 3, B),
	random(K, V, R), !,
	Cc is R+97,
	atom_codes(C0,[Cc]),
	(
	  B = 0 -> % nincs cimke
	  true
	;
	  B = 1 -> % ponalt cimke
	  C = C0
	;
	  % ponalt cimke
	  Cc = 97 -> % 'a' csak akkor lehet, ha nem mutat bele 'a' cimkeju el
	  (
	    \+ mutat_bele_a_el(N, Elek)->
	    C = not(C0)	% szabad negalni
	  ;
	    C = C0 % nem szabad ponalni
	  )
	;
	  C = not(C0)
	),
	csucscimke(Cs, Elek, K, V).
csucscimke(_, _, _, _).

% segedeljarasok

% novekvo_lista(A, N, L)
% L elemei eggyel novekvo sorozatot alkotnak
% A-tol N-1-ig
novekvo_lista(A, N, [A|As]):-
	A < N, !,
	A1 is A+1,
	novekvo_lista(A1, N, As).
novekvo_lista(_,_,[]).

% mutat_bele_a_el(+N, +Elek)
% N-be mutat 'a'-val cimkezett el
% az ellista Elek-ben van
mutat_bele_a_el(N, Elek) :-
	memberchk(el(a,csp(_),csp(N)), Elek).


graf2prolog(g(Elek, Csucsok), File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	bb_put(role_num,0),
	bb_put(concept_num,0),
	bb_put(individual_num,0),
	format('between(A,B,A).~n',[]),
	format('between(A,B,I):- A < B, A1 is A+1, between(A1,B,I).~n~n',[]),
	format('p(X) :- role_a(X, Y), role_a(Y, Z), not_a(Z), kPatricide(Y, X).~n',[]),
	format('kPatricide(X, _) :- a(X).~n',[]),
	format('kPatricide(Z, X) :- role_a(X, Y), role_a(Y, Z), kPatricide(Y, X).~n~n',[]),
	format('run(Time) :-  statistics(runtime,[A|_]), ((between(1,1000,_), findall(X,p(X),_), fail) ; true), statistics(runtime,[B|_]), Time is B-A.~n~n',[]),
	kiir_pelek(Elek),
	kiir_pcsucsok(Csucsok),
	close(Stream),
	set_output(Output),
	bb_get(role_num,RN),
	bb_get(concept_num,CN),
	bb_get(individual_num,IN),
	IN0 is IN+CN,
	format('Role assertions: ~w~n',[RN]),
	format('Concept assertions: ~w~n',[CN]),
	format('Individual num: ~w~n',[IN0]).

graf2xsb(g(Elek, Csucsok), File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	bb_put(role_num,0),
	bb_put(concept_num,0),
	bb_put(individual_num,0),
	format('between(A,B,A).~n',[]),
	format('between(A,B,I):- A < B, A1 is A+1, between(A1,B,I).~n~n',[]),
	format('p(X) :- role_a(X, Y), role_a(Y, Z), not_a(Z), kPatricide(Y, X).~n',[]),
	format(':- table kPatricide/2.~n',[]),
	format('kPatricide(X, _) :- a(X).~n',[]),
	format('kPatricide(Z, X) :- role_a(X, Y), role_a(Y, Z), kPatricide(Y, X).~n~n',[]),
	format('run :-  statistics(reset), ((between(1,1000,_), findall(X,p(X),_), abolish_all_tables, fail) ; true), statistics.~n~n',[]),
	kiir_pelek(Elek),
	kiir_pcsucsok(Csucsok),
	close(Stream),
	set_output(Output).

kiir_pelek([]).
kiir_pelek([el(Cimke,csp(A),csp(B))|Es]) :-
	(
	  var(Cimke)->
	  true
	;
	  bb_get(role_num,Num),
	  Num1 is Num+1,
	  bb_put(role_num,Num1),
	  format('role_~w(i~w,i~w).~n',[Cimke,A,B])
	),
	kiir_pelek(Es).

kiir_pcsucsok([]).
kiir_pcsucsok([csp(N)-Cimke|Cs]) :-
	(
	  var(Cimke)->
	  bb_get(individual_num,Num),
	  Num1 is Num+1,
	  bb_put(individual_num,Num1)
	;
	  Cimke = not(C) ->
	  bb_get(concept_num,Num),
	  Num1 is Num+1,
	  bb_put(concept_num,Num1),
	  format('not_~w(i~w).~n',[C,N])
	;
	  bb_get(concept_num,Num),
	  Num1 is Num+1,
	  bb_put(concept_num,Num1),
	  format('~w(i~w).~n',[Cimke,N])
	),
	kiir_pcsucsok(Cs).

graf2racer(g(Elek, Csucsok), File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	format('(in-abox test)~n~n',[]),
	kiir_relek(Elek),
	kiir_rcsucsok(Csucsok),
	format('(all-aboxes)~n',[]),
	format('(concept-instances (some role_a (and a (some role_a (not a)))))~n',[]),
	close(Stream),
	set_output(Output).

kiir_relek([]).
kiir_relek([el(Cimke,csp(A),csp(B))|Es]) :-
	(
	  var(Cimke)->
	  true
	;
	  format('(related i~w i~w role_~w)~n',[A,B,Cimke])
	),
	kiir_relek(Es).

kiir_rcsucsok([]).
kiir_rcsucsok([csp(N)-Cimke|Cs]) :-
	(
	  var(Cimke)->
	  true
	;
	  Cimke = not(C) ->
	  format('(instance i~w (not ~w))~n',[N,C])
	;
	  format('(instance i~w ~w)~n',[N,Cimke])
	),
	kiir_rcsucsok(Cs).

graf2owl(g(Elek, Csucsok), File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	format('<?xml version="1.0"?>~n',[]),
	format('<rdf:RDF~n',[]),
	format('  xml:base = "http://www.cs.bme.hu/vima9000"~n',[]),
	format('  xmlns = "http://www.cs.bme.hu/vima9000#"~n',[]),
	format('  xmlns:rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"~n',[]),
	format('  xmlns:rdfs = "http://www.w3.org/2000/01/rdf-schema#"~n',[]),
	format('  xmlns:owl = "http://www.w3.org/2002/07/owl#">~n~n',[]),
	format('<owl:Ontology rdf:about=""/>~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_a" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_b" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_c" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_d" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_e" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_f" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_g" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_h" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_i" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="role_j" />~n',[]),
	format('<owl:Class rdf:ID="a" />~n~n',[]),
	format('<owl:Class rdf:ID="nota">~n',[]),
	format('  <owl:complementOf>~n',[]),
	format('     <owl:Class rdf:about="#a"/>~n',[]),
	format('  </owl:complementOf>~n',[]),
	format('</owl:Class>~n~n',[]),
	format('<owl:Class rdf:ID="Jo">~n',[]),
	format('  <owl:equivalentClass>~n',[]),
	format('    <owl:Restriction>~n',[]),
	format('      <owl:onProperty rdf:resource="#role_a" />~n',[]),
	format('      <owl:someValuesFrom>~n',[]),
	format('      <owl:Class>~n',[]),
	format('        <owl:intersectionOf rdf:parseType="Collection">~n',[]),
	format('          <owl:Class rdf:about="#a" />~n',[]),
	format('          <owl:Restriction>~n',[]),
	format('            <owl:onProperty rdf:resource="#role_a" />~n',[]),
	format('            <owl:someValuesFrom rdf:resource="#nota"/>~n',[]),
	format('          </owl:Restriction>~n',[]),
	format('        </owl:intersectionOf>~n',[]),
	format('      </owl:Class>~n',[]),
	format('      </owl:someValuesFrom>~n',[]),
	format('    </owl:Restriction>~n',[]),
	format('  </owl:equivalentClass>~n',[]),
	format('</owl:Class>~n~n',[]),
	kiir_oelek(Elek),
	kiir_ocsucsok(Csucsok),
	format('</rdf:RDF>~n',[]),
	close(Stream),
	set_output(Output).

kiir_oelek([]).
kiir_oelek([el(Cimke,csp(A),csp(B))|Es]) :-
	(
	  var(Cimke)->
	  true
	;
	  format('<owl:Thing rdf:ID="i~w">~n',[A]),
	  format('  <role_~w rdf:resource="#i~w" />~n',[Cimke,B]),
	  format('</owl:Thing>~n~n',[])
	),
	kiir_oelek(Es).

kiir_ocsucsok([]).
kiir_ocsucsok([csp(N)-Cimke|Cs]) :-
	(
	  var(Cimke)->
	  true
	;
	  Cimke = not(C) ->
	  format('<not~w rdf:ID="i~w"/>~n',[C,N])
	;
	  format('<~w rdf:ID="i~w" />~n',[Cimke,N])
	),
	kiir_ocsucsok(Cs).

graf2protein(g(Elek, Csucsok), File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	kiir_protelek(Elek),
	kiir_protcsucsok(Csucsok),
	format('inst(X,Y,Z) :- ~n',[]),
	format('role_a(X,Y), role_a(Y,Z), a(Y), ~~a(Z).~n~n',[]),
	format('?- protein_answer(iocaste(X,Y,Z)), inst(X,Y,Z).~n',[]),
	close(Stream),
	set_output(Output).

kiir_protelek([]).
kiir_protelek([el(Cimke,csp(A),csp(B))|Es]) :-
	(
	  var(Cimke)->
	  true
	;
	  format('role_~w(i~w,i~w).~n',[Cimke,A,B])
	),
	kiir_protelek(Es).

kiir_protcsucsok([]).
kiir_protcsucsok([csp(N)-Cimke|Cs]) :-
	(
	  var(Cimke)->
	  true
	;
	  Cimke = not(C) ->
	  format('~~~w(i~w).~n',[C,N])
	;
	  format('~w(i~w).~n',[Cimke,N])
	),
	kiir_protcsucsok(Cs).

graf2dig(g(Elek, Csucsok), File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	format('<?xml version="1.0" encoding="UTF-8"?>~n',[]),
	format('<tells uri="" xmlns="http://dl.kr.org/dig/2003/02/lang">~n',[]),
	format('    <defconcept name="Patricide"/>~n',[]),
	format('    <defconcept name="Good"/>~n',[]),
	format('    <equalc>~n',[]),
	format('        <catom name="Good"/>~n',[]),
	format('        <and>~n',[]),
	format('            <some>~n',[]),
	format('                <ratom name="hasChild"/>~n',[]),
	format('                <and>~n',[]),
	format('                    <catom name="Patricide"/>~n',[]),
	format('                    <some>~n',[]),
	format('                        <ratom name="hasChild"/>~n',[]),
	format('                        <not>~n',[]),
	format('                          <catom name="Patricide"/>~n',[]),
	format('                        </not>~n',[]),
	format('                    </some>~n',[]),
	format('                </and>~n',[]),
	format('            </some>~n',[]),
	format('        </and>~n',[]),
	format('    </equalc>~n~n',[]),
	format('    <defrole name="hasChild"/>~n',[]),
	kiir_delek(Elek),
	kiir_dcsucsok(Csucsok),
	format('</tells>~n',[]),
	close(Stream),
	set_output(Output).

kiir_delek([]).
kiir_delek([el(Cimke,csp(A),csp(B))|Es]) :-
	(
	  var(Cimke)->
	  true
	;
	  (
	    Cimke == a->
	    Cimke1 = hasChild
	  ;
	    atom_concat('role_', Cimke, Cimke1)
	  ),
	  format('<related>~n',[]),
	  format('<individual name="i~w"/>~n',[A]),
	  format('<ratom name="~w"/>~n',[Cimke1]),
	  format('<individual name="i~w"/>~n',[B]),
	  format('</related>~n',[])
	),
	kiir_delek(Es).

kiir_dcsucsok([]).
kiir_dcsucsok([csp(N)-Cimke|Cs]) :-
	(
	  var(Cimke)->
	  true
	;
	  Cimke = not(C) ->
	  (
	    C == a ->
	    C1 = 'Patricide'
	  ;
	    C1 = C
	  ),
  	  format('<instanceof>~n',[]),
	  format('  <individual name="i~w"/>~n',[N]),
	  format('  <not>~n',[]),
	  format('    <catom name="~w"/>~n',[C1]),
	  format('  </not>~n',[]),
	  format('</instanceof>~n',[])
	;
	  (
	    Cimke == a ->
	    Cimke1 = 'Patricide'
	  ;
	    Cimke1 = Cimke
	  ),
	  format('<instanceof>~n',[]),
	  format('  <individual name="i~w"/>~n',[N]),
	  format('  <catom name="~w"/>~n',[Cimke1]),
	  format('</instanceof>~n',[])
	),
	kiir_dcsucsok(Cs).